extern crate encoding;
extern crate rand;

use std::cmp;
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

use rand::Rng;

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

type KladenetsKeyboard = [Vec<char>; 2];


#[derive(Debug, Copy, Clone)]
struct LetterPos {
    hand: usize,
    finger: usize,
    pos: usize,
}

enum RollDirection {
    Start,
    Unknown,
    Positive,
    Negative,
}

type Presses = Vec<LetterPos>;
type OneLetterCases = Vec<LetterPos>;
type UncertainPresses = Vec<OneLetterCases>;
type PressesOneHand = Vec<LetterPos>;
type Roll = Vec<LetterPos>;
type RollSplitted = Vec<Vec<Vec<LetterPos>>>;

trait KeyboardParser {
    fn get_pos(&self, c: &char) -> Option<OneLetterCases>;
    fn get_letter(&self, p: &LetterPos) -> char;
    fn to_presses(&self, s: &String) -> UncertainPresses;
    fn parse(&self, s: &String) -> Vec<Vec<String>>;
    fn unparse(&self, p: &RollSplitted) -> Vec<Vec<String>>;
    fn unparse0(&self, p: &Presses) -> String;
    fn format_word(&self, parsed: &Vec<Vec<String>>) -> String;
    fn format_parsed_word(&self, s: &String) -> String;
    fn print_parsed_word(&self, s: &String);
    fn println_parsed_word(&self, s: &String);
}

fn calc_word_metric(word: &RollSplitted) -> f64 {
    let mut bad_count = 0;
    let mut press_combinations = 0;
    for hand in word {
        for (pos, roll) in with_position(hand.iter()) {
            press_combinations += 1;
            match pos {
                Pos::First => { },
                Pos::Only => { },
                _ => {
                    bad_count += roll.len();    
                }
            }
        }
    }

    bad_count as f64 * 100.0 + press_combinations as f64
}

fn split_hands(positions: &Presses) -> Vec<PressesOneHand> {
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

#[derive(Clone, Debug)]
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

    let mut iterations = 500000;

    for line in file.split('\n') {
        let mut content = line.split('\t');
        let word = continue_if_none!(content.next());
        let count = continue_if_none!(content.next()).parse::<usize>().unwrap();

        result.push(WordStatistic { word: word.to_string(), count: count });

        iterations -= 1;
        if iterations <= 0 {
            break;
        }
    }

    result
}

fn cut_statistics(stats: &Vec<WordStatistic>, size: usize) -> Vec<WordStatistic> {
    let mut result: Vec<WordStatistic> = vec![];

    for (index, stat) in stats.iter().enumerate() {
        if index > size {
            break;
        }
        result.push(stat.clone());
    }

    result
}

fn count_combinations(stats: &Vec<WordStatistic>, cs: usize, name: &String) {
    let mut combinations = HashMap::new();

    for stat in stats {
        let size = stat.word.chars().count();
        for index in 0..(size-1) {
            if index + cs < size {
                let combination: String = stat.word.clone().chars().skip(index).take(cs).collect();
                let ss = combinations.entry(combination).or_insert(0);
                *ss += stat.count;
            }
            
        }
    }

    let sorted_result = sort_rolls_hashmap(&combinations);

    let write_file = File::create(format!("out/{}_{}_combinations.txt", name, cs)).unwrap();
    let mut writer = BufWriter::new(&write_file);
    for i in sorted_result {
        writeln!(&mut writer, "{}\t{}", i.0, i.1).unwrap();
    }
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
       (word[0][0].chars().count() == 1 || 
        word[1][0].chars().count() == 1) {
        result = false;
    }

    if word.len() == 3 && 
       (word[0][0].chars().count() == 1 &&
        word[1][0].chars().count() == 2 && 
        word[2][0].chars().count() == 1) {
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

    let stat_size = stats.len() as f64;
    let stat_size_percent_count = stat_size as usize / 1000;
    for (index, stat) in stats.iter().enumerate() {
        if index > 100 && index % stat_size_percent_count == 0 {
            //print!("\r {:5.1}%, word: {:10}", index as f64 / stat_size * 100.0, index);
        }
        let hand_mas = keyboard.parse(&stat.word);
        // if index < 100 {
        //     println!("{}: {}", stat.word, keyboard.format_parsed_word(&stat.word));
        // }
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
    //println!("\r{:40}", "Done");

    (sort_rolls_hashmap(&good_rolls), sort_rolls_hashmap(&bad_rolls))
}

fn is_similiar_words(a: &String, b: &String) -> bool {
    let asize = a.chars().count();
    let bsize = b.chars().count();
    let max_size = cmp::max(asize, bsize);
    let min_size = cmp::min(asize, bsize);

    if min_size+2 <= max_size {
        return false
    }
    if max_size < 3 {
        return false;
    }

    let substr_size = max_size - 2;

    let a_substr: String = a.chars().take(substr_size).collect();
    let b_substr: String = b.chars().take(substr_size).collect();
    
    a_substr == b_substr
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
    'main: for stat in stats {
        let hand_mas = keyboard.parse(&stat.word);
        if is_alternation_word(&hand_mas) {
            if alternation_mas.len() < alternation_count {
                for word in &alternation_mas {
                    if is_similiar_words(word, &stat.word) {
                        continue 'main;
                    }
                }
                alternation_mas.push(stat.word.clone());
            }
        } else if is_supa_pupa_roll_word(&hand_mas) {
            if !is_with_triple_fourthle_roll(&hand_mas) {
                if roll2_mas.len() < roll2_count {
                    for word in &roll2_mas {
                        if is_similiar_words(&word, &stat.word) {
                            continue 'main;
                        }
                    }
                    roll2_mas.push(stat.word.clone());
                }
            } else {
                if roll3_mas.len() < roll3_count {
                    for word in &roll3_mas {
                        if is_similiar_words(&word, &stat.word) {
                            continue 'main;
                        }
                    }
                    roll3_mas.push(stat.word.clone());
                }
            }
        }

        if roll2_mas.len() >= roll2_count &&
           roll3_mas.len() >= roll3_count &&
           alternation_mas.len() >= alternation_count {
            break 'main;
        }
    }

    roll2_mas.sort();
    roll3_mas.sort();
    alternation_mas.sort();

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

fn count_rolls_sum(rolls_sorted: &Vec<(String, usize)>) -> (usize, f64) {
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

    (all_sum, rolls)
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

    let (all_sum, rolls) = count_rolls_sum(rolls_sorted);

    println!("{} rolls count in layout: {:.1}%", name, rolls * 100.0);

    (all_sum, rolls)
}

fn parse_rolls(p: &Presses) -> RollSplitted {
    let mut result: RollSplitted = vec![];

    let hand_splitted = split_hands(&p);
    for hand in hand_splitted {
        result.push(split_rolls(hand));
    }

    result
}

fn recurse_travel(up: &UncertainPresses, pos: &usize, mut result: (f64, usize, RollSplitted), mut word: &mut Presses, keyboard: &Keyboard) -> (f64, usize, RollSplitted) {
    if pos < &up.len() {
        for letter in &up[pos.clone()] {
            word[pos.clone()] = *letter;
            if result.1 > 100 {
                print!("skip");
                return result
            }
            result = recurse_travel(&up, &(pos+1), result, &mut word, keyboard);
        }
    } else {
        let parsed = parse_rolls(&word.clone());
        let gradue = calc_word_metric(&parsed);
        // println!("{:5.0} - {}", gradue, keyboard.format_word(&keyboard.unparse(&parsed)));
        if gradue < result.0 {
            result = (gradue, result.1+1, parsed);
        }
    }
    result
}

fn certain_presses(up: &UncertainPresses, keyboard: &Keyboard) -> RollSplitted {
    let mut result = (10000000.0, 0, vec![]);
    let mut word = vec![LetterPos {hand: 0, finger: 0, pos: 0}; up.len()];
    result = recurse_travel(&up, &0, result, &mut word, &keyboard);

    result.2.clone()
}

fn select_best_rolls(rolls: &Vec<RollSplitted>) -> RollSplitted {
    let mut rolls_with_gradue = vec![];
    for roll in rolls {
        rolls_with_gradue.push((roll.clone(), calc_word_metric(roll)))
    }
    rolls_with_gradue.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(1.cmp(&1)));

    rolls_with_gradue.first().unwrap().0.clone()
}

impl KeyboardParser for Keyboard {
    fn get_pos(&self, c: &char) -> Option<OneLetterCases> {
        let mut result = vec![];

        for (hand_no, hand) in self.iter().enumerate() {
            for (finger_no, finger) in hand.iter().enumerate() {
                for (letter_no, letter) in finger.iter().enumerate() {
                    if letter == c {
                        result.push(LetterPos { hand: hand_no, finger: finger_no, pos: letter_no });
                    }
                }
            }
        }
        
        if result.len() == 0 {
            return None;
        } else {
            return Some(result);
        }
    }

    fn get_letter(&self, p: &LetterPos) -> char {
        self[p.hand][p.finger][p.pos]
    }

    fn to_presses(&self, s: &String) -> UncertainPresses {
        let mut positions = vec![];
        for c in s.chars() {
            match self.get_pos(&c) {
                Some(pos) => positions.push(pos), 
                None => println!("Error when trying to find pos of letter '{}'", c)
            }
        }
        positions
    }

    fn unparse(&self, p: &RollSplitted) -> Vec<Vec<String>> {
        let mut result = vec![];

        for hand in p {
            let mut ss = vec![];
            for roll in hand {
                let mut s = String::new();
                for letter in roll {
                    s.push(self.get_letter(letter));
                }
                ss.push(s);
            }
            result.push(ss);
        }

        result
    }

    fn unparse0(&self, p: &Presses) -> String {
        let mut s = String::new();
        for letter in p {
            s.push(self.get_letter(letter));
        }
        s
    }

    fn parse(&self, s: &String) -> Vec<Vec<String>> {
        let uncertain_presses = self.to_presses(s);
        let best_roll = certain_presses(&uncertain_presses, &self);
        // if certain_presses_mas.len() > 1 {
        //     println!("Pressess after certain: ");
        //     for p in certain_presses_mas {
        //         println!("    {}", self.format_word(&self.unparse(&parse_rolls(&p))));
        //     }
        //     println!("    Best roll: {}", self.format_word(&self.unparse(&best_roll)));
        // }
        self.unparse(&best_roll)
    }

    fn format_word(&self, parsed: &Vec<Vec<String>>) -> String {
        let mut result = String::new();
        for hand in parsed {
            result += "[";
            result += &hand.join("·");
            result += "]";
        }
        result.to_string()
    }

    fn format_parsed_word(&self, s: &String) -> String {
        self.format_word(&self.parse(s))
    }

    fn print_parsed_word(&self, s: &String) {
        print!("{}", self.format_parsed_word(s));
    }
    fn println_parsed_word(&self, s: &String) {
        self.print_parsed_word(s);
        println!("");
    }
}

fn eval_one_handed_kladenets(kb: &KladenetsKeyboard, stats: &Vec<WordStatistic>) -> f64 {
    enum PolysymbolicState {
        Start,
        Constontant,
        Vowel,
        VowelAfterConstontant,
    }
    let mut global_chords = 0;
    let mut letters = 0;
    for (index, stat) in stats.iter().enumerate() {
        if index > 10000 {
            break;
        }

        let mut chords = 0;
        let mut state = PolysymbolicState::Start;
        for symbol in stat.word.chars() {
            match kb[0].iter().find(|&&x| x == symbol) {
                Some(_s) => {
                    match state {
                        PolysymbolicState::Start => {
                            state = PolysymbolicState::Constontant;
                        },
                        PolysymbolicState::Constontant => {
                            chords += 1;
                            state = PolysymbolicState::Constontant;
                        },
                        PolysymbolicState::Vowel => {
                            chords += 1;
                            state = PolysymbolicState::Constontant;
                        },
                        PolysymbolicState::VowelAfterConstontant => {
                            chords += 1;
                            state = PolysymbolicState::Constontant;
                        },
                    }
                },
                None => {
                    match kb[1].iter().find(|&&x| x == symbol) {
                        Some(_s) => {
                            match state {
                                PolysymbolicState::Start => {
                                    state = PolysymbolicState::Vowel;
                                },
                                PolysymbolicState::Constontant => {
                                    state = PolysymbolicState::VowelAfterConstontant;
                                },
                                PolysymbolicState::Vowel => {
                                    chords += 1;
                                    state = PolysymbolicState::Vowel;
                                },
                                PolysymbolicState::VowelAfterConstontant => {
                                    chords += 1;
                                    state = PolysymbolicState::Vowel;
                                },
                            }
                        },
                        None => {
                            println!("wtf?");
                        }
                    }
                }
            }
        }
        chords += 1;

        letters += stat.count * (stat.word.chars().count()+1);
        global_chords += chords * stat.count;
    }

    let one_hand_kladenets = (letters as f64)/(global_chords as f64);
    one_hand_kladenets
}

fn mutate(mut keyboard: KladenetsKeyboard, rng: &mut rand::rngs::ThreadRng) -> KladenetsKeyboard {
    if rng.gen_range(0, 10) == 0 {
        if keyboard[0].len() > 0 && keyboard[1].len() > 0 {
            if rng.gen_range(0, 2) == 0 {
                let elem = keyboard[0].pop().unwrap();
                keyboard[1].push(elem);
            } else {
                let elem = keyboard[1].pop().unwrap();
                keyboard[0].push(elem);
            }
        } else if keyboard[0].len() == 0 {
            let elem = keyboard[1].pop().unwrap();
            keyboard[0].push(elem);
        } else if keyboard[1].len() == 0 {
            let elem = keyboard[0].pop().unwrap();
            keyboard[1].push(elem);
        }
    } else {
        let a = rng.gen_range(0, keyboard[0].len());
        let b = rng.gen_range(0, keyboard[1].len());

        let tmp = keyboard[0][a];
        keyboard[0][a] = keyboard[1][b];
        keyboard[1][b] = tmp;
    }

    keyboard
}

fn manymutate(mut keyboard: KladenetsKeyboard, rng: &mut rand::rngs::ThreadRng) -> KladenetsKeyboard {
    for _ in 0..100 {
        keyboard = mutate(keyboard, rng);
    }
    keyboard
}

fn find_optimal_kladenets_layout(stats: &Vec<WordStatistic>) -> (KladenetsKeyboard, f64) {
    let kb: KladenetsKeyboard = [vec!['а', 'е', 'ё', 'д', 'о', 'у', 'ы', 'ь', 'ю', 'я', 'н', 'п', 'р', 'с', 'ж', 'ф', 'х', 'ц', 'ч', 'ш', 'щ', 'ъ', 'э'], vec!['б', 'в', 'г', 'и', 'т', 'з', 'й', 'к', 'л', 'м']];

    let mut rng = rand::thread_rng();

    let populationSize = 50;
    let generations = 50;
    let evolutionsCount = 5;
    let bestCount = 5;
    let bestChildrenCount = populationSize/bestCount - 1;

    let mut best = (kb.clone(), eval_one_handed_kladenets(&kb, stats));
    for evolution in 0..evolutionsCount {
        println!("New evolution {}", evolution);
        // Инициализируем популяцию
        let mut population: Vec<KladenetsKeyboard> = vec![];
        for _ in 0..populationSize {
            population.push(manymutate(kb.clone(), &mut rng));
        }
        for generation in 0..generations {
            let mut evaledPopulation: Vec<(KladenetsKeyboard, f64)> = population.iter().map(|k| (k.clone(), eval_one_handed_kladenets(k, &stats))).collect();
            if evaledPopulation[0].1 > best.1 {
                best = evaledPopulation[0].clone();
                println!("Found new best: {:?}, {}", best.0, best.1);
            }
            println!("New generation {}, best: {}", generation, evaledPopulation[0].1);
            evaledPopulation.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap().reverse());
            population = vec![];

            for best in 0..bestCount {
                population.push(evaledPopulation[best].0.clone());
                for _ in 0..bestChildrenCount {
                    population.push(mutate(evaledPopulation[best].0.clone(), &mut rng));
                }
            }
        }
    }
    best
}

fn count_vowels() {
    let russian_stats = read_books_statistics("words/russian.txt");
    let mut pinky_spaces = 0;
    let mut thumb_spaces = 0;
    let kladenets_vowels = vec!['а', 'е', 'ё', 'и', 'о', 'у', 'ы', 'ь', 'ю', 'я'];
    for stat in russian_stats {
        if kladenets_vowels.contains(&stat.word.chars().last().unwrap()) {
            pinky_spaces += stat.count;
        } else {
            thumb_spaces += stat.count;
        }

        break;
    }

    let all_spaces = pinky_spaces + thumb_spaces;
    println!("мизинчиковый пробел: {}%, тамбовый пробел: {}%", pinky_spaces as f64/all_spaces as f64 * 100.0, thumb_spaces as f64/all_spaces as f64 * 100.0);
}

fn eval_kladenets_misc() {
    let kladenets_keyboards: Vec<(String, KladenetsKeyboard)> = vec![
        ("standard".to_string(), [
            vec!['б', 'в', 'г', 'д', 'ж', 'з', 'й', 'к', 'л', 'м', 'н', 'п', 'р', 'с', 'т', 'ф', 'х', 'ц', 'ч', 'ш', 'щ', 'ъ', 'э'],
            vec!['а', 'е', 'ё', 'и', 'о', 'у', 'ы', 'ь', 'ю', 'я']
        ])
    ];

    let russian_stats = read_books_statistics("words/russian.txt");

    let kb = &kladenets_keyboards[0].1;

    let one_hand_kladenets = eval_one_handed_kladenets(&kb, &russian_stats);
    println!("{}, {}", one_hand_kladenets, one_hand_kladenets * 2.0);

    // Ищем лучшую раскладку Кладенца при помощи генетического алгоритма
    let best = find_optimal_kladenets_layout(&russian_stats);
    println!("{:?}, {}", best.0, best.1);
}

fn count_good_rolls_percent(creature_keyboard: &Keyboard, evolution_stats: &Vec<WordStatistic>) -> f64 {
    let (good, bad) = count_rolls(&creature_keyboard, &evolution_stats);
    let (all_good_sum, good_rolls) = count_rolls_sum(&good);
    let (all_bad_sum, bad_rolls) = count_rolls_sum(&bad);

    let good_rolls = all_good_sum as f64 * good_rolls;
    let bad_rolls = all_bad_sum as f64 * bad_rolls;

    let good_percent = (good_rolls) / (all_good_sum + all_bad_sum) as f64;

    good_percent
}

struct KeyboardPos (usize, usize, usize);

fn swap(keyboard: &mut Keyboard, a: KeyboardPos, b: KeyboardPos) {
    let firstEmpty = keyboard[a.0][a.1].len() == 0;
    let secondEmpty = keyboard[b.0][b.1].len() == 0;
    if !firstEmpty && !secondEmpty {
        let tmp = keyboard[a.0][a.1][a.2];
        keyboard[a.0][a.1][a.2] = keyboard[b.0][b.1][b.2];
        keyboard[b.0][b.1][b.2] = tmp;
    } else if firstEmpty && secondEmpty {
        // do nothing
    } else if firstEmpty && !secondEmpty {
        stole(keyboard, a, b);
    } else if !firstEmpty && secondEmpty {
        stole(keyboard, b, a);
    }
}

fn stole(keyboard: &mut Keyboard, from: KeyboardPos, to: KeyboardPos) {
    let letter = keyboard[from.0][from.1].swap_remove(from.2);
    keyboard[to.0][to.1].push(letter);
}

fn gen_random_keyboard_pos(keyboard: &Keyboard, rng: &mut rand::rngs::ThreadRng) -> KeyboardPos {
    let mut result = KeyboardPos(0, 0, 0);
    result.0 = rng.gen_range(0, keyboard.len());
    result.1 = rng.gen_range(0, keyboard[result.0].len());
    result.2 = rng.gen_range(0, keyboard[result.0][result.1].len());
    result
}

fn mutate_keyboard(keyboard: &Keyboard, rng: &mut rand::rngs::ThreadRng) -> Keyboard {
    let mut result = keyboard.clone();

    let a = gen_random_keyboard_pos(&result, rng);
    let b = gen_random_keyboard_pos(&result, rng);
    swap(&mut result, a, b);
    // println!("{:?}\n{:?}", keyboard, result);

    result
}

fn calc_best_rolls_layout_by_evolution(creature_keyboard: &Keyboard, evolution_stats: &Vec<WordStatistic>) {
    let mut rng = rand::thread_rng();

    let population_size = 50;
    let generations = 100;
    let evolutions_count = 1;
    let best_count = 5;
    let best_children_count = population_size/best_count - 1;

    let mut best = (creature_keyboard.clone(), count_good_rolls_percent(&creature_keyboard, &evolution_stats));
    for evolution in 0..evolutions_count {
        println!("New evolution {}", evolution);
        // Инициализируем популяцию
        let mut population: Vec<Keyboard> = vec![];
        for _ in 0..population_size {
            population.push(mutate_keyboard(&creature_keyboard, &mut rng));
        }
        for generation in 0..generations {
            let mut evaled_population: Vec<(Keyboard, f64)> = population.iter().map(|k| (k.clone(), count_good_rolls_percent(&k, &evolution_stats))).collect();
            if evaled_population[0].1 > best.1 {
                best = evaled_population[0].clone();
                println!("Found new best: {:?}, {}", best.0, best.1);
            }
            println!("New generation {}, best: {}", generation, evaled_population[0].1);
            evaled_population.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap().reverse());
            population = vec![];

            for best in 0..best_count {
                population.push(evaled_population[best].0.clone());
                for _ in 0..best_children_count {
                    population.push(mutate_keyboard(&evaled_population[best].0, &mut rng));
                }
            }
        }
    }

    println!("Overall best: {:?}, {}", best.0, best.1);
}

fn calc_best_rolls_layout() {
    let english_stats = read_books_statistics("words/english.txt");
    let evolution_stats_eng = cut_statistics(&english_stats, 10000);

    let creature_keyboard_eng_1 = [
        vec![
            vec!['q', 'a', 'z'], 
            vec!['w', 's', 'x'], 
            vec!['e', 'd', 'c'], 
            vec!['r', 'f', 'v', 'b', 'g', 't'], 
        ],
        vec![
            vec!['y', 'h', 'n', 'u', 'j', 'm'], 
            vec!['i', 'k', '.'], 
            vec!['o', 'l', '.'], 
            vec!['p', '.', '.'], 
        ]
    ];

    let creature_keyboard_eng_2 = [
        vec![
            vec!['q', 'a', 'z'], 
            vec!['w', 's', 'x'], 
            vec!['e', 'd', 'c'], 
            vec!['r', 'f', 'v', 'b', 'g', 't'], 
        ],
        vec![
            vec!['y', 'h', 'n', 'u', 'j', 'm'], 
            vec!['i', 'k', 't'], 
            vec!['o', 'l', 'a'], 
            vec!['p', 'o', 'e'], 
        ]
    ];

    calc_best_rolls_layout_by_evolution(&creature_keyboard_eng_1, &evolution_stats_eng);
    calc_best_rolls_layout_by_evolution(&creature_keyboard_eng_2, &evolution_stats_eng);

    let russian_stats = read_books_statistics("words/russian.txt");
    let evolution_stats_rus = cut_statistics(&russian_stats, 10000);

    let creature_keyboard_rus_1 = [
        vec![
            vec!['й', 'ф', 'я', 'ё', '.', '.'], 
            vec!['ц', 'ы', 'ч'], 
            vec!['у', 'в', 'с'], 
            vec!['к', 'а', 'м', 'е', 'п', 'и'], 
        ],
        vec![
            vec!['з', 'ж', 'х', 'э', 'ъ', '.'], 
            vec!['щ', 'д', 'ю'], 
            vec!['ш', 'л', 'б'], 
            vec!['г', 'о', 'ь', 'н', 'р', 'т'], 
        ]
    ];

    let creature_keyboard_rus_2 = [
        vec![
            vec!['й', 'ф', 'я', 'ё', 'о', 'а'], 
            vec!['ц', 'ы', 'ч'], 
            vec!['у', 'в', 'с'], 
            vec!['к', 'а', 'м', 'е', 'п', 'и'], 
        ],
        vec![
            vec!['з', 'ж', 'х', 'э', 'ъ', 'т'], 
            vec!['щ', 'д', 'ю'], 
            vec!['ш', 'л', 'б'], 
            vec!['г', 'о', 'ь', 'н', 'р', 'т'], 
        ]
    ];

    calc_best_rolls_layout_by_evolution(&creature_keyboard_rus_1, &evolution_stats_rus);
    calc_best_rolls_layout_by_evolution(&creature_keyboard_rus_2, &evolution_stats_rus);
}

fn main() {
    calc_best_rolls_layout();
    return;

    count_vowels();
    return;

    eval_kladenets_misc();
    return;

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
        ("эволюция 1".to_string(), [
            vec![
                vec!['у', 'ы', 'я', 'о', 'ь', '.'], 
                vec!['ш', 'с', 'г'], 
                vec!['б', 'т', 'п'], 
                vec!['й', 'к', 'э', 'ч', 'в', 'х']
            ], 
            vec![
                vec!['ф', 'ж', '.', 'м', 'ц', 'д'], 
                vec!['з', 'е', 'ю'], 
                vec!['ё', 'а', 'и'], 
                vec!['н', 'л', 'щ', 'ъ', 'р', '.']
            ]
        ]),
        ("эволюция 2".to_string(), [
            vec![
                vec!['в', 'м', 'я', 'ё', 'у', 'ь'], 
                vec!['а', 'ы', 'о'], 
                vec!['д', 'к', 'т'], 
                vec!['ц', 'э', 'ш', 'б', 'с', 'ч']
            ], 
            vec![
                vec!['ж', 'з', 'й', 'ф', 'т', 'п'], 
                vec!['ю', 'г', 'и'], 
                vec!['е', 'а', 'о'], 
                vec!['щ', 'л', 'х', 'р', 'н', 'ъ']
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
        ("double_dvorak".to_string(), [
            vec![
                vec!['s', 'p', 't'], 
                vec!['d', 'h', 'n'], 
                vec!['k', 'o', 'q'], 
                vec!['f', 'u', 'j', 'i', 'e', 'x'], 
            ],
            vec![
                vec!['y', 'o', 'b', 'g', 'a', 'v'], 
                vec!['c', 'e', 'w'], 
                vec!['r', 'h', 'm'], 
                vec!['l', 't', 'z'], 
            ]
        ]),
        ("workman".to_string(), [
            vec![
                vec!['q', 'a', 'z'], 
                vec!['d', 's', 'x'], 
                vec!['r', 'h', 'm'], 
                vec!['w', 't', 'c', 'b', 'g', 'v'], 
            ],
            vec![
                vec!['j', 'y', 'k', 'f', 'n', 'l'], 
                vec!['u', 'e'], 
                vec!['p', 'o'], 
                vec!['i'], 
            ]
        ]),
        ("colemak".to_string(), [
            vec![
                vec!['q', 'a', 'z'], 
                vec!['w', 'r', 'x'], 
                vec!['f', 's', 'c'], 
                vec!['p', 't', 'v', 'g', 'd', 'b'], 
            ],
            vec![
                vec!['j', 'h', 'k', 'l', 'n', 'm'], 
                vec!['u', 'e'], 
                vec!['y', 'i'], 
                vec!['o'], 
            ]
        ]),
        ("evolutionary 1".to_string(), [
            vec![
                vec!['b', 'r', 'x'], 
                vec!['l', 'm', 'k'], 
                vec!['e', 'u', 'z'], 
                vec!['d', 'y', 'v', 'p', 'g', 'q']
            ], 
            vec![
                vec!['f', 'h', 'c', 'n', 'j', 's'], 
                vec!['a', 'o'], 
                vec!['t', 'w'], 
                vec!['i']
            ]
        ]),
        ("evolutionary 2".to_string(), [
            vec![
                vec!['a', 'o', '.'], 
                vec!['n', 'h', 'u'], 
                vec!['w', 'c', 'p'], 
                vec!['q', 'f', '.', 'j', 'g', 't']
            ], 
            vec![
                vec!['l', 'x', 'm', 'r', 'z', 'v'], 
                vec!['i', 'k', 'y'], 
                vec!['.', 'e', '.'], 
                vec!['b', 's', 'd']
            ]
        ]),
        ("evolutionary 3".to_string(), [
            vec![
                vec!['z', 'a', 'u'], 
                vec!['n', 'm', 'l'], 
                vec!['e', 'q', 'o'], 
                vec!['y', 'd', 'j', 'g', 't', 'p']
            ], 
            vec![
                vec!['w', 'h', 'b', 'c', 'v', 'f'], 
                vec!['a', 'k', 't'], 
                vec!['o', 'e', 'i'], 
                vec!['x', 'r', 's']
            ]
        ]),
    ];

    let russian_stats = read_books_statistics("words/russian.txt");
    for i in 1..5 {
        count_combinations(&russian_stats, i, &"russian".to_string());
    }

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
        println!("Percent of good presses in layout: {:.1}%", (all_good_sum as f64)/((all_good_sum + all_bad_sum) as f64) * 100.0);
        println!("Percent of rolls in layout: {:.1}%", (all_good_sum as f64 * good_rolls + all_bad_sum as f64 * bad_rolls)/((all_good_sum + all_bad_sum) as f64) * 100.0);
        println!("");

        write_supa_pupa_words(&keyboard, &russian_stats, &("out/rolls/".to_owned() + &name + ".txt"));
    }

    let english_stats = read_books_statistics("words/english.txt");

    for i in 1..5 {
        count_combinations(&english_stats, i, &"english".to_string());
    }

    // let dvorak2 = &english_keyboards[0];
    // dvorak2.1.parse(&"that".to_string());
    // return;

    for (name, keyboard) in &english_keyboards {
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
        println!("Percent of good presses in layout: {:.1}%", (all_good_sum as f64)/((all_good_sum + all_bad_sum) as f64) * 100.0);
        println!("Percent of rolls in layout: {:.1}%", (all_good_sum as f64 * good_rolls + all_bad_sum as f64 * bad_rolls)/((all_good_sum + all_bad_sum) as f64) * 100.0);
        println!("");

        write_supa_pupa_words(&keyboard, &english_stats, &("out/rolls/".to_owned() + &name + ".txt"));
    }
}
