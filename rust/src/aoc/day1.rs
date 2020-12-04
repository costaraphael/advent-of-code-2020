use itertools::Itertools;
use std::fs;
use std::io;
use std::io::prelude::*;
use std::iter::Iterator;

const INPUT: &str = "../inputs/day1.txt";

fn read_input() -> impl Iterator<Item = usize> {
    let input = fs::File::open(INPUT).unwrap();
    let file_contents = io::BufReader::new(input);

    file_contents.lines().map(|s| s.unwrap().parse().unwrap())
}

#[allow(dead_code)]
pub fn run_part_1() {
    let res: Vec<usize> = read_input()
        .combinations(2)
        .find(|p| p.iter().sum::<usize>() == 2020)
        .unwrap();

    println!("{:?}", res.iter().product::<usize>());
}

#[allow(dead_code)]
pub fn run_part_2() {
    let res: Vec<usize> = read_input()
        .combinations(3)
        .find(|p| p.iter().sum::<usize>() == 2020)
        .unwrap();

    println!("{:?}", res.iter().product::<usize>());
}
