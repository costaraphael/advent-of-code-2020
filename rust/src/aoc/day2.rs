use regex::Regex;
use std::fs;
use std::io;
use std::io::prelude::*;
use std::str::FromStr;

#[derive(Debug)]
struct Range {
    first: usize,
    last: usize,
}

#[derive(Debug)]
struct PasswordAndRules {
    password: String,
    rule: Range,
    character: char,
}

lazy_static! {
    static ref LINE_REGEX: Regex =
        Regex::new(r"(?P<min>\d+)-(?P<max>\d+) (?P<char>.): (?P<password>.+)").unwrap();
}

impl FromStr for PasswordAndRules {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let captures = LINE_REGEX.captures(s).unwrap();

        Ok(PasswordAndRules {
            password: captures["password"].to_string(),
            rule: Range {
                first: captures["min"].parse().unwrap(),
                last: captures["max"].parse().unwrap(),
            },
            character: captures["char"].chars().next().unwrap(),
        })
    }
}

const INPUT: &str = "../inputs/day2.txt";

fn read_input() -> impl Iterator<Item = PasswordAndRules> {
    let input = fs::File::open(INPUT).unwrap();
    let file_contents = io::BufReader::new(input);

    file_contents.lines().map(|s| s.unwrap().parse().unwrap())
}

#[allow(dead_code)]
pub fn run_part_1() {
    let res: usize = read_input()
        .filter(|password_and_rules| {
            let char_count = password_and_rules
                .password
                .chars()
                .filter(|c| *c == password_and_rules.character)
                .count();

            char_count >= password_and_rules.rule.first
                && char_count <= password_and_rules.rule.last
        })
        .count();

    println!("{:?}", res);
}

#[allow(dead_code)]
pub fn run_part_2() {
    let res: usize = read_input()
        .filter(|password_and_rules| {
            let chars: Vec<_> = password_and_rules.password.chars().collect();

            let char_count = vec![password_and_rules.rule.first, password_and_rules.rule.last]
                .iter()
                .map(|n| chars[n - 1])
                .filter(|c| *c == password_and_rules.character)
                .count();

            char_count == 1
        })
        .count();

    println!("{:?}", res);
}
