use itertools::Itertools;
use regex::Regex;
use std::fs;
use std::io;
use std::io::prelude::*;
use std::str::FromStr;

lazy_static! {
    static ref PASSPORT_ID_REGEX: Regex = Regex::new(r"^\d{9}$").unwrap();
    static ref HAIR_COLOR_REGEX: Regex = Regex::new(r"^#[0-9a-f]{6}$").unwrap();
    static ref HEIGHT_REGEX: Regex = Regex::new(r"^(\d+)(in|cm)$").unwrap();
}

const INPUT: &str = "../inputs/day4.txt";

#[derive(Debug)]
struct Passport {
    byr: Option<usize>,
    iyr: Option<usize>,
    eyr: Option<usize>,
    hgt: Option<String>,
    hcl: Option<String>,
    ecl: Option<String>,
    pid: Option<String>,
    cid: Option<String>,
}

impl Default for Passport {
    fn default() -> Self {
        Self {
            byr: None,
            iyr: None,
            eyr: None,
            hgt: None,
            hcl: None,
            ecl: None,
            pid: None,
            cid: None,
        }
    }
}

impl FromStr for Passport {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut passport = Passport::default();

        s.split(" ")
            .for_each(|part| match part.split(":").collect::<Vec<_>>()[..] {
                ["byr", byr] => passport.byr = byr.parse().map(|byr| Some(byr)).unwrap_or(None),
                ["iyr", iyr] => passport.iyr = iyr.parse().map(|iyr| Some(iyr)).unwrap_or(None),
                ["eyr", eyr] => passport.eyr = eyr.parse().map(|eyr| Some(eyr)).unwrap_or(None),
                ["hgt", hgt] => passport.hgt = Some(hgt.parse().unwrap()),
                ["hcl", hcl] => passport.hcl = Some(hcl.parse().unwrap()),
                ["ecl", ecl] => passport.ecl = Some(ecl.parse().unwrap()),
                ["pid", pid] => passport.pid = Some(pid.parse().unwrap()),
                ["cid", cid] => passport.cid = Some(cid.parse().unwrap()),
                _ => panic!("invalid passport"),
            });

        Ok(passport)
    }
}

fn read_input() -> Vec<Passport> {
    let input = fs::File::open(INPUT).unwrap();
    let file_contents = io::BufReader::new(input);
    let groups = file_contents
        .lines()
        .map(|l| l.unwrap())
        .group_by(|l| l == "");

    groups
        .into_iter()
        .filter(|(is_empty, _)| !*is_empty)
        .map(|(_, mut lines)| lines.join(" ").parse().unwrap())
        .collect()
}

fn validate_required(passport: &Passport) -> bool {
    passport.byr.is_some()
        && passport.iyr.is_some()
        && passport.eyr.is_some()
        && passport.hgt.is_some()
        && passport.hcl.is_some()
        && passport.ecl.is_some()
        && passport.pid.is_some()
}

pub fn run_part_1() {
    let passports = read_input();

    let count = passports.iter().filter(|p| validate_required(p)).count();

    println!("{:?}", count)
}

fn validate_in_range(number: usize, min: usize, max: usize) -> bool {
    min <= number && number <= max
}

fn validate_height(height: &String) -> bool {
    if let Some(captures) = HEIGHT_REGEX.captures(height) {
        if let Ok(amount) = captures[1].parse::<usize>() {
            let unit = &captures[2];

            return (unit == "cm" && validate_in_range(amount, 150, 193))
                || (unit == "in" && validate_in_range(amount, 59, 76));
        }
    }

    return false;
}

pub fn run_part_2() {
    let passports = read_input();

    let count = passports
        .iter()
        .filter(|p| validate_required(p))
        .filter(|p| validate_in_range(p.byr.unwrap(), 1920, 2002))
        .filter(|p| validate_in_range(p.iyr.unwrap(), 2010, 2020))
        .filter(|p| validate_in_range(p.eyr.unwrap(), 2020, 2030))
        .filter(|p| validate_height(p.hgt.as_ref().unwrap()))
        .filter(|p| HAIR_COLOR_REGEX.is_match(p.hcl.as_ref().unwrap()))
        .filter(|p| {
            vec!["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
                .contains(&p.ecl.as_ref().unwrap().as_str())
        })
        .filter(|p| PASSPORT_ID_REGEX.is_match(p.pid.as_ref().unwrap()))
        .count();

    println!("{:?}", count)
}
