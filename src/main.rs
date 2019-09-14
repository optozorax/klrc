extern crate encoding;

use std::fs;
use std::{
    fs::File,
    io::{BufWriter},
};
use regex::Regex;
use std::collections::HashMap;

use std::io::prelude::*;

use encoding::all::WINDOWS_1251;
use encoding::{Encoding, DecoderTrap};

use std::mem;

// Thanks to red75prime https://play.rust-lang.org/?version=stable&mode=release&edition=2018&gist=e2f92e287e61cfab6e3bc6426a229aab

#[derive(Debug)]
enum Pos {
    First,
    Middle,
    Last,
    Only,
}

enum Prev<T> {
    First(T),
    Rest(T),
    None,
}

struct WithPosition<I: Iterator> {
    prev: Prev<I::Item>,
    iter: I,
}

impl<I: Iterator> Iterator for WithPosition<I> {
    type Item = (Pos, I::Item);

    fn next(&mut self) -> Option<Self::Item> {
        match mem::replace(&mut self.prev, Prev::None) {
            Prev::First(x) => {
                if let Some(next) = self.iter.next() {
                    self.prev = Prev::Rest(next);
                    Some((Pos::First, x))
                } else {
                    Some((Pos::Only, x))
                }
            }
            Prev::Rest(x) => {
                if let Some(next) = self.iter.next() {
                    self.prev = Prev::Rest(next);
                    Some((Pos::Middle, x))
                } else {
                    Some((Pos::Last, x))
                }
            }
            Prev::None => None,
        }
    }
}

fn with_position<I: Iterator>(mut iter: I) -> WithPosition<I> {
    if let Some(first) = iter.next() {
        WithPosition {
            prev: Prev::First(first),
            iter,
        }
    } else {
        WithPosition {
            prev: Prev::None,
            iter,
        }
    }
}

type Keyboard = [Vec<Vec<char>>; 2];

trait KeyboardParser {
    fn get_pos(&self, c: &char) -> Result<LetterPos, ()>;
    fn parse(&self, s: &String) -> Vec<Vec<String>>;
    fn format_parsed_word(&self, s: &String) -> String;
    fn print_parsed_word(&self, s: &String);
    fn println_parsed_word(&self, s: &String);
}


#[derive(Debug, Copy, Clone)]
struct LetterPos {
    hand: usize,
    finger: usize,
}

enum RollDirection {
    Start,
    Unknown,
    Positive,
    Negative,
}

type Presses = Vec<LetterPos>;
type PressesOneHand = Vec<LetterPos>;
type Roll = Vec<LetterPos>;

fn split_hands(positions: Presses) -> Vec<PressesOneHand> {
    let mut result: Vec<PressesOneHand> = vec![];
    let mut current_hand = std::usize::MAX;
    for pos in positions {
        if current_hand != pos.hand {
            result.push(vec![])
        }
        match result.last_mut() {
            Some(last) => last.push(pos.clone()),
            None => result.push(vec![pos.clone()])
        }
        current_hand = pos.hand.clone()
    }
    result
}

fn split_rolls(positions: PressesOneHand) -> Vec<Roll> {
    let mut result: Vec<Roll> = vec![];
    let mut dir = RollDirection::Start;
    let mut last_finger = 0;
    for pos in positions {
        match dir {
            RollDirection::Start => {
                dir = RollDirection::Unknown;
                result.push(vec![pos]);
            },
            RollDirection::Unknown => {
                if last_finger == pos.finger {
                    dir = RollDirection::Unknown;
                    result.push(vec![pos]);
                } else if last_finger < pos.finger {
                    dir = RollDirection::Positive;
                    result.last_mut().unwrap().push(pos);
                } else {
                    dir = RollDirection::Negative;
                    result.last_mut().unwrap().push(pos);
                }
            },
            RollDirection::Positive => {
                if last_finger < pos.finger {
                    result.last_mut().unwrap().push(pos);
                } else {
                    dir = RollDirection::Unknown;
                    result.push(vec![pos]);
                }
            },
            RollDirection::Negative => {
                if last_finger > pos.finger {
                    result.last_mut().unwrap().push(pos);
                } else {
                    dir = RollDirection::Unknown;
                    result.push(vec![pos]);
                }
            },
        }
        last_finger = pos.finger;
    }
    result
}

fn read_russian_file_to_string(s: &str) -> String {
    let mut file = File::open(s).expect(&format!("File {} not found", s));
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).unwrap();

    let contents = match fs::read_to_string(s) {
        Ok(readed) => readed,
        Err(_) => WINDOWS_1251
            .decode(&buffer, DecoderTrap::Strict)
            .expect(&format!("File {} encoded not in UTF-8 and not in 1251, can't read it as russian file.", s))
    };

    contents
}

fn delete_html_tags(s: String) -> String {
    let reg = Regex::new("<[^>]*>").unwrap();
    reg.replace_all(&s, "").to_string()
}

fn delete_non_russian_letters(s: String) -> String {
    let reg = Regex::new("[^а-яё ]").unwrap();
    reg.replace_all(&s, "").to_string()   
}

fn delete_non_english_letters(s: String) -> String {
    let reg = Regex::new("[^a-z ]").unwrap();
    reg.replace_all(&s, "").to_string()   
}

fn read_books_in_dir(dir: String, words: &mut HashMap<String, usize>, is_russian: bool) {
    match fs::read_dir(dir.clone()) {
        Ok(paths) => {
            for path in paths {
                let new_dir = path.unwrap().path().into_os_string().into_string().unwrap();
                read_books_in_dir(new_dir, words, is_russian);
            }
        },
        Err(_) => {
            let filename = dir;

            let text = read_russian_file_to_string(&filename);

            let text = text.to_lowercase();
            let mut text = delete_html_tags(text);
            if is_russian {
                text = delete_non_russian_letters(text);
            } else {
                text = delete_non_english_letters(text);
            }
            
            for word in text.split_whitespace() {
                let word_copy: String = word.to_string();
                let stat = words.entry(word_copy).or_insert(0);
                *stat += 1;
            }
        },
    }
}

fn read_books(dir: String, out: String, is_russian: bool) {
    let mut words = HashMap::new();

    read_books_in_dir(dir, &mut words, is_russian);

    let write_file = File::create(&out).unwrap();
    let mut writer = BufWriter::new(&write_file);

    let words_sorted = sort_words_hashmap(&words);
    for (word, count) in &words_sorted {
        writeln!(&mut writer, "{}\t{}", word, count).unwrap();
    }
}

struct WordStatistic {
    word: String,
    count: usize
}

macro_rules! continue_if_none {
    ($res:expr) => {
        match $res {
            Some(val) => val,
            None => continue,
        }
    };
}

fn read_books_statistics(filename: &str) -> Vec<WordStatistic> {
    let mut result = vec![];

    let file = fs::read_to_string(filename).unwrap();

    for line in file.split('\n') {
        let mut content = line.split('\t');
        let word = continue_if_none!(content.next());
        let count = continue_if_none!(content.next()).parse::<usize>().unwrap();

        result.push(WordStatistic { word: word.to_string(), count: count });
    }

    result
}

fn sort_words_hashmap(hashmap: &HashMap<String, usize>) -> Vec<(String, usize)> {
    let mut sorted = Vec::new();

    for (string, count) in hashmap {
        sorted.push((string.clone(), count.clone()));
    }

    sorted.sort_by(|a, b| if b.1 == a.1 { return a.0.cmp(&b.0); } else { return b.1.cmp(&a.1) });

    sorted
}

fn sort_rolls_hashmap(hashmap: &HashMap<String, usize>) -> Vec<(String, usize)> {
    let mut sorted = Vec::new();

    for (string, count) in hashmap {
        sorted.push((string.clone(), count.clone()));
    }

    sorted.sort_by(|a, b| return (b.0.chars().count() * b.1).cmp(&(a.0.chars().count() * a.1)));

    sorted
}

fn is_supa_pupa_roll_word(word: &Vec<Vec<String>>) -> bool {
    let mut result = true;

    for (pos, hand) in with_position(word.iter()) {
        use Pos::*;

        if hand.len() != 1 {
            return false;
        }

        match pos {
            First => {},
            Last => {},
            Middle => {
                if hand[0].chars().count() == 1 {
                    return false;
                }
            },
            Only => {
                return false;
            },
        }
    }

    if word.len() == 2 && 
       word[0].len() == 1 && 
       word[1].len() == 2 {
        result = false;
    }

    if word.len() == 2 && 
       (word[0].len() == 1 || 
        word[1].len() == 1) {
        result = false;
    }

    result
}

fn is_alternation_word(word: &Vec<Vec<String>>) -> bool {
    if word.len() < 3 {
        return false;
    }

    for hand in word {
        if hand.len() != 1 {
            return false;
        }
        for roll in hand {
            if roll.chars().count() != 1 {
                return false;
            }
        }
    }

    return true;
}

fn is_with_triple_fourthle_roll(word: &Vec<Vec<String>>) -> bool {
    for hand in word {
        for roll in hand {
            if roll.chars().count() > 2 {
                return true;
            }
        }
    }
    return false;
}

fn count_rolls(keyboard: &Keyboard, stats: &Vec<WordStatistic>) -> (Vec<(String, usize)>, Vec<(String, usize)>) {
    let mut good_rolls = HashMap::new();
    let mut bad_rolls = HashMap::new();

    for stat in stats {
        let hand_mas = keyboard.parse(&stat.word);
        for roll_mas in hand_mas {
            let mut first = true;
            for roll in roll_mas {
                if first {
                    first = false;
                    let ss = good_rolls.entry(roll).or_insert(0);
                    *ss += stat.count;
                } else {
                    let ss = bad_rolls.entry(roll).or_insert(0);
                    *ss += stat.count;
                }
            }
        }
    }

    (sort_rolls_hashmap(&good_rolls), sort_rolls_hashmap(&bad_rolls))
}

fn write_supa_pupa_words(keyboard: &Keyboard, stats: &Vec<WordStatistic>, out_file: &String) {
    let roll2_count = 100;
    let roll3_count = 50;
    let alternation_count = 100;
    // let parsed_count = 50_000;

    let mut roll2_mas = vec![];
    let mut roll3_mas = vec![];
    let mut alternation_mas = vec![];

    let write_file = File::create(out_file).unwrap();
    let mut writer = BufWriter::new(&write_file);

    // Собираем слова для словарей
    for stat in stats {
        let hand_mas = keyboard.parse(&stat.word);
        if is_alternation_word(&hand_mas) {
            if alternation_mas.len() < alternation_count {
                alternation_mas.push(stat.word.clone());
            }
        } else if is_supa_pupa_roll_word(&hand_mas) {
            if !is_with_triple_fourthle_roll(&hand_mas) {
                if roll2_mas.len() < roll2_count {
                    roll2_mas.push(stat.word.clone());
                }
            } else {
                if roll3_mas.len() < roll3_count {
                    roll3_mas.push(stat.word.clone());
                }
            }
        }
    }

    // Выводим первые 100 слов с перекатами только из двух букв
    writeln!(&mut writer, "First {} words with maximum alternation:", alternation_count).unwrap();
    writeln!(&mut writer, "{}", alternation_mas.join(" ")).unwrap();
    writeln!(&mut writer, "").unwrap();

    // Выводим первые 100 слов с перекатами только из двух букв
    writeln!(&mut writer, "First {} words with roll of size 2:", roll2_count).unwrap();
    writeln!(&mut writer, "{}", roll2_mas.join(" ")).unwrap();
    writeln!(&mut writer, "").unwrap();

    // Выводим первые 50 слов с перекатами из трёх или даже четырх букв
    writeln!(&mut writer, "First {} words with roll of size more than 3:", roll3_count).unwrap();
    writeln!(&mut writer, "{}", roll3_mas.join(" ")).unwrap();
    writeln!(&mut writer, "").unwrap();

    // Выводим парсинг всех использованных слов
    writeln!(&mut writer, "Parsing of all this words:").unwrap();
    let words_to_print = vec![
        ("Alternation words:", alternation_mas),
        ("Roll2 words:", roll2_mas),
        ("Roll3 words:", roll3_mas),
    ];
    for (text, word_mas) in words_to_print {
        writeln!(&mut writer, "{}", text).unwrap();
        for word in word_mas {
            writeln!(&mut writer, "{}\t{}", word, keyboard.format_parsed_word(&word)).unwrap();
        }
        writeln!(&mut writer, "").unwrap();
    }

    // Выводим парсинг всех слов в принципе и их частотность
    // writeln!(&mut writer, "First {} words parsed:", parsed_count).unwrap();
    // writeln!(&mut writer, "Word\tParsed\tFrequency").unwrap();
    // for (index, stat) in stats.iter().enumerate() {
    //     if index > parsed_count {
    //         break;
    //     }
    //     writeln!(&mut writer, "{}\t{}\t{}", stat.word, keyboard.format_parsed_word(&stat.word), stat.count).unwrap();
    // }
}

fn print_rolls(rolls_sorted: &Vec<(String, usize)>, name: &String, is_print_rolls: bool) -> (usize, f64) {
    if is_print_rolls {
        for (index, (roll, count)) in rolls_sorted.iter().enumerate() {
            println!("{}\t{}", roll, count);
            if index > 40 {
                break;
            }
        }
    }

    let mut roll_sum = 0;
    let mut all_sum = 0;

    for (roll, count) in rolls_sorted {
        let len = roll.chars().count();
        all_sum += count * len;
        if len > 1 {
            roll_sum += count * len;
        }
    }

    let rolls = (roll_sum as f64)/(all_sum as f64);
    println!("{} rolls count in layout: {}%", name, rolls * 100.0);

    (all_sum, rolls)
}

impl KeyboardParser for Keyboard {
    fn get_pos(&self, c: &char) -> Result<LetterPos, ()> {
        for (hand_no, hand) in self.iter().enumerate() {
            for (finger_no, finger) in hand.iter().enumerate() {
                for letter in finger {
                    if letter == c {
                        return Ok(LetterPos { hand: hand_no, finger: finger_no });
                    }
                }
            }
        }
        return Err(());
    }

    fn parse(&self, s: &String) -> Vec<Vec<String>> {
        let mut positions: Presses = vec![];
        for c in s.chars() {
            match self.get_pos(&c) {
                Ok(pos) => positions.push(pos), 
                Err(_) => println!("Error when trying to find pos of letter '{}'", c)
            }
        }

        let hand_splitted = split_hands(positions);
        // println!("splitted by hand: {:?}", hand_splitted);

        let mut c = s.chars();

        let mut result: Vec<Vec<String>> = vec![];
        for presses in hand_splitted {
            result.push(vec![]);

            let back = result.last_mut().unwrap();
            let splitted_roll = split_rolls(presses);
            
            for roll in splitted_roll {
                let mut roll_str = String::new();
                for _ in roll {
                    match c.next() {
                        Some(c) => roll_str.push(c),
                        None => {

                        },
                    }
                }
                back.push(roll_str);
            }
        }

        result
    }

    fn format_parsed_word(&self, s: &String) -> String {
        let mut result = String::new();
        let parsed = self.parse(s);
        for hand in parsed {
            result += "[";
            result += &hand.join("·");
            result += "]";
        }
        result.to_string()
    }

    fn print_parsed_word(&self, s: &String) {
        print!("{}", self.format_parsed_word(s));
    }
    fn println_parsed_word(&self, s: &String) {
        self.print_parsed_word(s);
        println!("");
    }
}


fn main() {
    read_books("./books/russian/".to_string(), "./out/russian_words.txt".to_string(), true);
    read_books("./books/english/".to_string(), "./out/english_words.txt".to_string(), false);

    let russian_keyboards: Vec<(String, Keyboard)> = vec![
        ("йцукен".to_string(), [
            vec![
                vec!['й', 'ф', 'я', 'ё'], 
                vec!['ц', 'ы', 'ч'], 
                vec!['у', 'в', 'с'], 
                vec!['к', 'а', 'м', 'е', 'п', 'и'], 
            ],
            vec![
                vec!['з', 'ж', 'х', 'э', 'ъ'], 
                vec!['щ', 'д', 'ю'], 
                vec!['ш', 'л', 'б'], 
                vec!['г', 'о', 'ь', 'н', 'р', 'т'], 
            ]
        ]),
        ("optozorax".to_string(), [ 
            vec![
                vec!['й', 'к', 'я'], 
                vec!['ц', 'м', 'ч'], 
                vec!['у', 'в', 'с'], 
                vec!['е', 'а', 'и', 'ф', 'п', 'ы'], 
            ],
            vec![
                vec!['з', 'ж', 'х', 'ъ', 'э', 'ё'], 
                vec!['н', 'д', 'ю'], 
                vec!['т', 'л', 'б'], 
                vec!['г', 'о', 'ь', 'щ', 'р', 'ш'], 
            ]
        ]),
        ("тестовый".to_string(), [
            vec![
                vec!['й', 'к', 'я'], 
                vec!['ц', 'м', 'ч'], 
                vec!['у', 'в', 'п'], 
                vec!['р', 'а', 'г', 'ф', 'щ', 'ы'], 
            ],
            vec![
                vec!['с', 'ж', 'х', 'ъ', 'э', 'ё'], 
                vec!['н', 'д', 'ю'], 
                vec!['т', 'л', 'б'], 
                vec!['е', 'о', 'ь', 'з', 'и', 'ш'], 
            ]
        ]),
        ("kanazei".to_string(), [
            vec![
                vec!['ё', 'ы'], 
                vec!['ь', 'э', 'ъ'], 
                vec!['у', 'а'], 
                vec!['и', 'о', 'ю', 'я'], 
                vec!['е'], 
            ],
            vec![
                vec!['ц', 'ш', 'й', 'д', 'х', 'ч'], 
                vec!['з', 'с', 'ж'], 
                vec!['щ', 'к', 'т', 'п'], 
                vec!['в', 'н', 'м', 'б', 'л', 'г', 'ф'], 
                vec!['р'], 
            ]
        ]),
        ("диктор".to_string(), [
            vec![
                vec!['ц', 'у', 'ф', 'ё'], 
                vec!['ь', 'и', 'э', 'ъ'], 
                vec!['я', 'е', 'х'], 
                vec!['о', 'ы', 'а', 'ю'], 
            ],
            vec![
                vec!['з', 'л', 'б', 'в', 'н', 'м'], 
                vec!['к', 'т', 'п'], 
                vec!['д', 'с', 'г'], 
                vec!['ч', 'р', 'ж', 'й', 'ш', 'щ'], 
            ]
        ]),
        ("зубачёв".to_string(), [
            vec![
                vec!['ф', 'г', 'ш', 'ё'], 
                vec!['ы', 'и', 'ь', 'ъ'], 
                vec!['а', 'е', 'ю'], 
                vec!['я', 'о', 'у', 'э'], 
            ],
            vec![
                vec!['й', 'л', 'б', 'м', 'т', 'д'], 
                vec!['р', 'с', 'в'], 
                vec!['п', 'н', 'к'], 
                vec!['х', 'з', 'ч', 'ц', 'ж', 'щ'], 
            ]
        ]),
        ("bouncepaw".to_string(), [
            vec![
                vec!['ж', 'з', 'в', 'й'], 
                vec!['п', 'с', 'ч'], 
                vec!['р', 'т', 'м'], 
                vec!['д', 'н', 'к', 'ф'], 
            ],
            vec![
                vec!['ь', 'е', 'а', 'щ', 'ё', 'ъ', 'ц'], 
                vec!['я', 'о', 'ы', 'ю', 'ш'], 
                vec!['у', 'и', 'б'], 
                vec!['х', 'л', 'г', 'э'], 
            ]
        ]),
    ];

    let english_keyboards: Vec<(String, Keyboard)> = vec![
        ("qwerty".to_string(), [
            vec![
                vec!['q', 'a', 'z'], 
                vec!['w', 's', 'x'], 
                vec!['e', 'd', 'c'], 
                vec!['r', 'f', 'v', 'b', 'g', 't'], 
            ],
            vec![
                vec!['y', 'h', 'n', 'u', 'j', 'm'], 
                vec!['i', 'k'], 
                vec!['o', 'l'], 
                vec!['p'], 
            ]
        ]),
        ("dvorak".to_string(), [
            vec![
                vec!['a'], 
                vec!['o', 'q'], 
                vec!['e', 'j'], 
                vec!['p', 'u', 'k', 'y', 'i', 'x'], 
            ],
            vec![
                vec!['f', 'd', 'b', 'g', 'h', 'm'], 
                vec!['c', 't', 'w'], 
                vec!['r', 'n', 'v'], 
                vec!['l', 's', 'z'], 
            ]
        ]),
        ("capewell_0.9.3".to_string(), [
            vec![
                vec!['a', 'x'], 
                vec!['y', 'e', 'z'], 
                vec!['m', 'r', 'c'], 
                vec!['d', 's', 'v', 'f', 'g'], 
            ],
            vec![
                vec!['j', 'b', 'k', 'p', 't', 'w'], 
                vec!['l', 'n', 'h'], 
                vec!['u', 'i'], 
                vec!['q', 'o'], 
            ]
        ]),
    ];

    let russian_stats = read_books_statistics("words/russian.txt");

    for (name, keyboard) in russian_keyboards {
        println!("Layout: {}", name);

        println!("{:?}", keyboard.get_pos(&'м'));
        keyboard.println_parsed_word(&"саломам".to_string());
        keyboard.println_parsed_word(&"седло".to_string());
        keyboard.println_parsed_word(&"бывало".to_string());
        keyboard.println_parsed_word(&"радоваться".to_string());
        keyboard.println_parsed_word(&"фываолдж".to_string());
        keyboard.println_parsed_word(&"сейчас".to_string());
        keyboard.println_parsed_word(&"чирик".to_string());
        keyboard.println_parsed_word(&"нефтеперерабатывающий".to_string());
        keyboard.println_parsed_word(&"непосредственный".to_string());
        keyboard.println_parsed_word(&"страстный".to_string());
        keyboard.println_parsed_word(&"искусство".to_string());

        keyboard.println_parsed_word(&"сейчас".to_string());
        keyboard.println_parsed_word(&"бывало".to_string());
        keyboard.println_parsed_word(&"слова".to_string());

        let (good, bad) = count_rolls(&keyboard, &russian_stats);
        let (all_good_sum, good_rolls) = print_rolls(&good, &"Good".to_string(), false);
        let (all_bad_sum, bad_rolls) = print_rolls(&bad, &"Bad ".to_string(), false);
        println!("Percent of good presses in layout: {}%", (all_good_sum as f64)/((all_good_sum + all_bad_sum) as f64) * 100.0);
        println!("Percent of rolls in layout: {}%", (all_good_sum as f64 * good_rolls + all_bad_sum as f64 * bad_rolls)/((all_good_sum + all_bad_sum) as f64) * 100.0);
        println!("");

        write_supa_pupa_words(&keyboard, &russian_stats, &("out/rolls/".to_owned() + &name + ".txt"));
    }

    let english_stats = read_books_statistics("words/english.txt");

    for (name, keyboard) in english_keyboards {
        println!("Layout: {}", name);

        println!("{:?}", keyboard.get_pos(&'f'));
        keyboard.println_parsed_word(&"hello".to_string());
        keyboard.println_parsed_word(&"should".to_string());
        keyboard.println_parsed_word(&"must".to_string());
        keyboard.println_parsed_word(&"doing".to_string());
        keyboard.println_parsed_word(&"institution".to_string());
        keyboard.println_parsed_word(&"right".to_string());
        keyboard.println_parsed_word(&"clear".to_string());

        let (good, bad) = count_rolls(&keyboard, &english_stats);
        let (all_good_sum, good_rolls) = print_rolls(&good, &"Good".to_string(), false);
        let (all_bad_sum, bad_rolls) = print_rolls(&bad, &"Bad ".to_string(), false);
        println!("Percent of good presses in layout: {}%", (all_good_sum as f64)/((all_good_sum + all_bad_sum) as f64) * 100.0);
        println!("Percent of rolls in layout: {}%", (all_good_sum as f64 * good_rolls + all_bad_sum as f64 * bad_rolls)/((all_good_sum + all_bad_sum) as f64) * 100.0);
        println!("");

        write_supa_pupa_words(&keyboard, &english_stats, &("out/rolls/".to_owned() + &name + ".txt"));
    }
}
