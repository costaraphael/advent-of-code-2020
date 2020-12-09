#![feature(iterator_fold_self)]
#![feature(str_split_once)]
#![feature(hash_set_entry)]

#[macro_use]
extern crate lazy_static;

mod aoc;

fn main() {
    aoc::day1::run_part_1();
    aoc::day1::run_part_2();
    aoc::day2::run_part_1();
    aoc::day2::run_part_2();
    aoc::day3::run_part_1();
    aoc::day3::run_part_2();
    aoc::day4::run_part_1();
    aoc::day4::run_part_2();
    aoc::day5::run_part_1();
    aoc::day5::run_part_2();
    aoc::day6::run_part_1();
    aoc::day6::run_part_2();
    aoc::day7::run_part_1();
    aoc::day7::run_part_2();
    aoc::day8::run_part_1();
    aoc::day8::run_part_2();
}
