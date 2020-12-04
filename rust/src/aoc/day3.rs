use std::fs;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
enum TreeMapItem {
    Tree,
    Empty,
}

#[derive(Debug)]
struct TreeMap {
    map: Vec<Vec<TreeMapItem>>,
    height: usize,
    width: usize,
}

impl FromStr for TreeMap {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let map: Vec<Vec<_>> = s
            .lines()
            .map(|line| {
                line.chars()
                    .map(|ch| match ch {
                        '#' => TreeMapItem::Tree,
                        '.' => TreeMapItem::Empty,
                        other => panic!(format!("invalid char {:?} found", other)),
                    })
                    .collect()
            })
            .collect();

        let height = map.len();
        let width = map[0].len();

        Ok(TreeMap { map, height, width })
    }
}

struct TreeMapIter<'a> {
    tree_map: &'a TreeMap,
    x_movement: usize,
    y_movement: usize,
    current_x: usize,
    current_y: usize,
}

impl TreeMap {
    fn iter(&self, x_movement: usize, y_movement: usize) -> TreeMapIter {
        TreeMapIter {
            tree_map: self,
            x_movement,
            y_movement,
            current_x: 0,
            current_y: 0,
        }
    }

    fn is_out_of_bounds(&self, y: usize) -> bool {
        self.height <= y
    }

    fn get(&self, x: usize, y: usize) -> &TreeMapItem {
        let actual_x = x % self.width;

        &self.map[y][actual_x]
    }
}

impl<'a> Iterator for TreeMapIter<'a> {
    type Item = &'a TreeMapItem;

    fn next(&mut self) -> Option<Self::Item> {
        if self.tree_map.is_out_of_bounds(self.current_y) {
            None
        } else {
            let item = self.tree_map.get(self.current_x, self.current_y);

            self.current_x += self.x_movement;
            self.current_y += self.y_movement;

            Some(item)
        }
    }
}

const INPUT: &str = "../inputs/day3.txt";

fn read_input() -> TreeMap {
    let input = fs::read_to_string(INPUT).unwrap();

    input.parse().unwrap()
}

#[allow(dead_code)]
pub fn run_part_1() {
    let tree_map = read_input();

    let result = tree_map
        .iter(3, 1)
        .filter(|i| *i == &TreeMapItem::Tree)
        .count();

    println!("{:?}", result);
}

#[allow(dead_code)]
pub fn run_part_2() {
    let tree_map = read_input();

    let result: usize = [[1, 1], [3, 1], [5, 1], [7, 1], [1, 2]]
        .iter()
        .map(|[x_movement, y_movement]| {
            tree_map
                .iter(*x_movement, *y_movement)
                .filter(|i| *i == &TreeMapItem::Tree)
                .count()
        })
        .product();

    println!("{:?}", result);
}
