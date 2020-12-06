use itertools::Itertools;
use std::collections::HashSet;
use std::fs;
use std::io;
use std::io::prelude::*;

const INPUT: &str = "../inputs/day6.txt";

fn read_input() -> Vec<Vec<HashSet<char>>> {
    let input = fs::File::open(INPUT).unwrap();
    let file_contents = io::BufReader::new(input);
    let groups = file_contents
        .lines()
        .map(|l| l.unwrap())
        .group_by(|l| l == "");

    groups
        .into_iter()
        .filter(|(is_empty, _)| !*is_empty)
        .map(|(_, group)| group.map(|g| g.chars().collect()).collect())
        .collect()
}

#[allow(dead_code)]
pub fn run_part_1() {
    let answers = read_input()
        .into_iter()
        .map(|group| {
            group
                .into_iter()
                .fold_first(|s1, s2| &s1 | &s2)
                .unwrap()
                .len()
        })
        .sum::<usize>();

    println!("{:?}", answers)
}

#[allow(dead_code)]
pub fn run_part_2() {
    let answers = read_input()
        .into_iter()
        .map(|group| {
            group
                .into_iter()
                .fold_first(|s1, s2| &s1 & &s2)
                .unwrap()
                .len()
        })
        .sum::<usize>();

    println!("{:?}", answers)
}
