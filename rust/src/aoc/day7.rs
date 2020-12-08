use itertools::Itertools;
use std::collections::HashMap;
use std::fs;

const INPUT: &str = "../inputs/day7.txt";

type BagRules = HashMap<String, HashMap<String, usize>>;

fn read_input() -> BagRules {
    let file_contents = fs::read_to_string(INPUT).unwrap();
    let mut rules = HashMap::new();

    file_contents
        .split(".\n")
        .filter(|l| l != &"")
        .map(|l| l.split(" bags contain ").collect::<Vec<_>>())
        .map(|l| {
            let outer_color = l[0].to_string();
            match l[1] {
                "no other bags" => (outer_color, HashMap::new()),
                other => (
                    outer_color,
                    other
                        .split(", ")
                        .map(|part| {
                            let mut part_iter = part.split(" ");
                            let amount = part_iter.next().unwrap().parse().unwrap();
                            let inner_color = part_iter
                                .take_while(|word| word != &"bags" && word != &"bag")
                                .join(" ");
                            (inner_color, amount)
                        })
                        .collect(),
                ),
            }
        })
        .for_each(|(key, inner_map)| {
            rules.insert(key, inner_map);
        });

    rules
}

fn container_bags(rules: &BagRules, color: &str) -> Vec<String> {
    rules
        .iter()
        .filter(|(_, inner_map)| inner_map.contains_key(color))
        .flat_map(|(container_color, _)| {
            let mut inner_containers = container_bags(rules, container_color);
            inner_containers.push(container_color.clone());
            inner_containers
        })
        .collect()
}

fn calculate_inner_bag_amount(rules: &BagRules, color: &str) -> usize {
    if let Some(inner_map) = rules.get(color) {
        inner_map
            .iter()
            .map(|(inner_color, bag_amount)| {
                bag_amount + bag_amount * calculate_inner_bag_amount(&rules, inner_color)
            })
            .sum::<usize>()
    } else {
        0
    }
}

#[allow(dead_code)]
pub fn run_part_1() {
    let rules = read_input();
    let result = container_bags(&rules, "shiny gold").len();

    println!("{:?}", result);
}

#[allow(dead_code)]
pub fn run_part_2() {
    let rules = read_input();
    let result = calculate_inner_bag_amount(&rules, "shiny gold");

    println!("{:?}", result);
}
