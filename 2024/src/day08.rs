use core::f64;
use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    isize,
};

use crate::{time, util};

pub fn run(it: util::InputType) {
    let input = read_input(it);
    time!("Part 1", println!("{}", part1(&input)));
    time!("Part 2", println!("{}", part2(&input)));
}

fn part1(input: &Input) -> usize {
    find_antinodes(input).len()
}

fn part2(input: &Input) -> usize {
    find_antinodes2(input).len()
}

fn find_antinodes(input: &Input) -> HashSet<Point> {
    let mut antinodes = HashSet::new();

    for (_, antennas) in input.antennas.iter() {
        let pairs = antennas.iter().combinations(2);
        for pair in pairs {
            if pair[0].x == pair[1].x {
                // vertical line
                let dist = pair[0].y.abs_diff(pair[1].y);

                let small_y = pair[0].y.min(pair[1].y);
                let big_y = pair[0].y.max(pair[1].y);

                let antinode1 = Point {
                    x: pair[0].x,
                    y: small_y - dist as isize,
                };
                if check_bounds(antinode1, input.map_bounds) {
                    antinodes.insert(antinode1);
                }

                let antinode2 = Point {
                    x: pair[0].x,
                    y: big_y + dist as isize,
                };
                if check_bounds(antinode2, input.map_bounds) {
                    antinodes.insert(antinode2);
                }
            } else if pair[0].y == pair[1].y {
                // Horizonal line
                let dist = pair[0].x.abs_diff(pair[1].x);

                let small_x = pair[0].x.min(pair[1].x);
                let big_x = pair[0].x.max(pair[1].x);

                let antinode1 = Point {
                    x: small_x - dist as isize,
                    y: pair[0].y,
                };
                if check_bounds(antinode1, input.map_bounds) {
                    antinodes.insert(antinode1);
                }

                let antinode2 = Point {
                    x: big_x + dist as isize,
                    y: pair[0].y,
                };
                if check_bounds(antinode2, input.map_bounds) {
                    antinodes.insert(antinode2);
                }
            } else {
                let slope = (pair[1].y - pair[0].y) as f64 / (pair[1].x - pair[0].x) as f64;
                let c = pair[0].y as f64 - (slope * pair[0].x as f64);

                let small_x = pair[0].x.min(pair[1].x);
                let big_x = pair[0].x.max(pair[1].x);
                let x_dist = big_x - small_x;

                let x1 = small_x - x_dist;
                let antinode1 = Point {
                    x: x1,
                    y: ((slope * x1 as f64) + c).round() as isize,
                };
                if check_bounds(antinode1, input.map_bounds) {
                    antinodes.insert(antinode1);
                }

                let x2 = big_x + x_dist;
                let antinode2 = Point {
                    x: x2,
                    y: ((slope * x2 as f64) + c).round() as isize,
                };
                if check_bounds(antinode2, input.map_bounds) {
                    antinodes.insert(antinode2);
                }
            }
        }
    }

    antinodes
}

fn find_antinodes2(input: &Input) -> HashSet<Point> {
    let mut antinodes = HashSet::new();

    for (_, antennas) in input.antennas.iter() {
        let pairs = antennas.iter().combinations(2);
        for pair in pairs {
            antinodes.insert(*pair[0]);
            antinodes.insert(*pair[1]);

            if pair[0].x == pair[1].x {
                // vertical line
                let dist = pair[0].y.abs_diff(pair[1].y) as isize;

                let small_y = pair[0].y.min(pair[1].y);
                let big_y = pair[0].y.max(pair[1].y);

                let mut antinode1 = Point {
                    x: pair[0].x,
                    y: small_y - dist,
                };
                while check_bounds(antinode1, input.map_bounds) {
                    antinodes.insert(antinode1);
                    antinode1.y -= dist;
                }

                let mut antinode2 = Point {
                    x: pair[0].x,
                    y: big_y + dist,
                };
                while check_bounds(antinode2, input.map_bounds) {
                    antinodes.insert(antinode2);
                    antinode2.y += dist;
                }
            } else if pair[0].y == pair[1].y {
                // Horizonal line
                let dist = pair[0].x.abs_diff(pair[1].x) as isize;

                let small_x = pair[0].x.min(pair[1].x);
                let big_x = pair[0].x.max(pair[1].x);

                let mut antinode1 = Point {
                    x: small_x - dist,
                    y: pair[0].y,
                };
                while check_bounds(antinode1, input.map_bounds) {
                    antinodes.insert(antinode1);
                    antinode1.x -= dist;
                }

                let mut antinode2 = Point {
                    x: big_x + dist,
                    y: pair[0].y,
                };
                while check_bounds(antinode2, input.map_bounds) {
                    antinodes.insert(antinode2);
                    antinode2.x += dist;
                }
            } else {
                let slope = (pair[1].y - pair[0].y) as f64 / (pair[1].x - pair[0].x) as f64;
                let c = pair[0].y as f64 - (slope * pair[0].x as f64);

                let small_x = pair[0].x.min(pair[1].x);
                let big_x = pair[0].x.max(pair[1].x);
                let x_dist = big_x - small_x;

                let x1 = small_x - x_dist;
                let mut antinode1 = Point {
                    x: x1,
                    y: ((slope * x1 as f64) + c).round() as isize,
                };
                while check_bounds(antinode1, input.map_bounds) {
                    antinodes.insert(antinode1);
                    antinode1.x -= x_dist;
                    antinode1.y = ((slope * antinode1.x as f64) + c).round() as isize;
                }

                let x2 = big_x + x_dist;
                let mut antinode2 = Point {
                    x: x2,
                    y: ((slope * x2 as f64) + c).round() as isize,
                };
                while check_bounds(antinode2, input.map_bounds) {
                    antinodes.insert(antinode2);
                    antinode2.x += x_dist;
                    antinode2.y = ((slope * antinode2.x as f64) + c).round() as isize;
                }
            }
        }
    }

    antinodes
}

fn check_bounds(p: Point, map_bounds: Point) -> bool {
    p.x <= map_bounds.x && p.x >= 0 && p.y <= map_bounds.y && p.y >= 0
}

#[derive(Debug)]
struct Input {
    map_bounds: Point,
    antennas: HashMap<char, Vec<Point>>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
struct Point {
    x: isize,
    y: isize,
}

fn read_input(it: util::InputType) -> Input {
    let input_lines: Vec<String> = util::read_file(util::Day::Day08, it)
        .lines()
        .map(|s| s.to_string())
        .collect();

    let map_bounds = Point {
        x: (input_lines.len() - 1) as isize,
        y: (input_lines[0].len() - 1) as isize,
    };

    let mut antennas: HashMap<char, Vec<Point>> = HashMap::new();
    for (y, l) in input_lines.iter().enumerate() {
        for (x, c) in l.chars().enumerate() {
            if c != '.' {
                antennas.entry(c).or_default().push(Point {
                    x: x as isize,
                    y: y as isize,
                });
            }
        }
    }

    Input {
        map_bounds,
        antennas,
    }
}

#[cfg(test)]
mod tests {
    use crate::day08::part1;
    use crate::day08::part2;
    use crate::day08::read_input;
    use crate::util;

    #[test]
    fn part1_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part1(&input), 14);
    }

    #[test]
    fn part1_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part1(&input), 336);
    }

    #[test]
    fn part2_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part2(&input), 34);
    }

    #[test]
    fn part2_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part2(&input), 1131);
    }
}
