use itertools::Itertools;
use std::collections::HashSet;
use std::fs;

const INPUT: &str = "../inputs/day8.txt";

#[derive(Debug, Clone, Copy)]
enum Instruction {
    Nop(isize),
    Jmp(isize),
    Acc(isize),
}

#[derive(Debug, Clone, Copy)]
enum ExecutionResult {
    InfiniteLoop(isize),
    Finished(isize),
}

type Program = Vec<Instruction>;

fn read_input() -> Program {
    let file_contents = fs::read_to_string(INPUT).unwrap();

    file_contents
        .split("\n")
        .filter(|l| l != &"")
        .map(|line| {
            let parts = line.split(" ").collect::<Vec<_>>();
            let arg = parts[1].parse::<isize>().unwrap();

            match parts[0] {
                "jmp" => Instruction::Jmp(arg),
                "nop" => Instruction::Nop(arg),
                "acc" => Instruction::Acc(arg),
                _ => panic!("invalid instruction"),
            }
        })
        .collect()
}

fn execute(program: &Program) -> ExecutionResult {
    let mut acc = 0;
    let mut idx: isize = 0;
    let mut seen = HashSet::new();

    while (idx as usize) < program.len() {
        if seen.contains(&idx) {
            return ExecutionResult::InfiniteLoop(acc);
        }

        seen.insert(idx);

        match program[idx as usize] {
            Instruction::Nop(_) => {
                idx += 1;
            }
            Instruction::Jmp(arg) => {
                idx += arg;
            }
            Instruction::Acc(arg) => {
                idx += 1;
                acc += arg;
            }
        };
    }

    ExecutionResult::Finished(acc)
}

#[allow(dead_code)]
pub fn run_part_1() {
    let program = read_input();

    println!("{:?}", execute(&program));
}

fn find_jmps_and_nops(program: &Program) -> Vec<usize> {
    program
        .iter()
        .positions(|instruction| match instruction {
            Instruction::Jmp(_) | Instruction::Nop(_) => true,
            Instruction::Acc(_) => false,
        })
        .collect()
}

fn swap_instruction(program: &Program, idx: usize) -> Program {
    let mut program = program.clone();

    match program[idx] {
        Instruction::Jmp(arg) => program[idx] = Instruction::Nop(arg),
        Instruction::Nop(arg) => program[idx] = Instruction::Jmp(arg),
        _ => {}
    };

    program
}

#[allow(dead_code)]
pub fn run_part_2() {
    let program = read_input();

    let result = find_jmps_and_nops(&program)
        .iter()
        .map(|idx| {
            let program = swap_instruction(&program, *idx);

            execute(&program)
        })
        .filter(|result| match result {
            ExecutionResult::InfiniteLoop(_) => false,
            ExecutionResult::Finished(_) => true,
        })
        .next()
        .unwrap();

    println!("{:?}", result);
}
