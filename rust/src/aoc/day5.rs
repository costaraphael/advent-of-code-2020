use itertools::Itertools;
use std::fs;
use std::io;
use std::io::prelude::*;

const INPUT: &str = "../inputs/day5.txt";

fn read_input() -> impl Iterator<Item = u16> {
    let input = fs::File::open(INPUT).unwrap();
    let file_contents = io::BufReader::new(input);

    file_contents.lines().map(|l| {
        u16::from_str_radix(
            l.unwrap()
                .chars()
                .map(|letter| match letter {
                    'B' | 'R' => '1',
                    'F' | 'L' => '0',
                    _ => panic!("invalid seat"),
                })
                .join("")
                .as_str(),
            2,
        )
        .unwrap()
    })
}

#[allow(dead_code)]
pub fn run_part_1() {
    let max_seat_id = read_input().max().unwrap();

    println!("{:?}", max_seat_id)
}

#[allow(dead_code)]
pub fn run_part_2() {
    let my_seat_id = read_input()
        .sorted()
        .tuple_windows()
        .filter(|(a, b)| *a == b - 2)
        .map(|(_, b)| b - 1)
        .next()
        .unwrap();

    println!("{:?}", my_seat_id)
}
