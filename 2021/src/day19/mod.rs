use std::collections::HashSet;

use regex::Regex;

use crate::{time, utils};

pub fn run(it: utils::InputType) {
    let scanners = read_input(it);
    time!("Part 1", println!("{}", part1(&scanners)));
}

fn part1(scanners: &[Scanner]) -> usize {
    let mut num_beacons = 0;
    let mut scanners_iter = scanners.iter();
    num_beacons += scanners_iter.next().unwrap().points.len();
    for s in scanners_iter {
        // FIXME it's at least 12 so this doesn't work.
        num_beacons += s.points.len() - 12;
    }

    num_beacons
}

fn read_input(it: utils::InputType) -> Vec<Scanner> {
    let header_regex = Regex::new(r"^--- scanner ([^ ]+) ---$").unwrap();
    let point_regex = Regex::new(r"^(-?\d+),(-?\d+),(-?\d+)$").unwrap();
    let input_str = utils::read_file(utils::Day::Day19, it);
    let mut scanners: Vec<Scanner> = Vec::new();
    for scanner_str in input_str.split("\n\n") {
        let mut points = HashSet::new();
        let mut lines_iter = scanner_str.lines();
        let header = lines_iter.next().unwrap();
        let header_caps = header_regex.captures(header).unwrap();
        for line in lines_iter {
            let point_caps = point_regex.captures(line).unwrap();
            let p = Point {
                x: point_caps.get(1).unwrap().as_str().parse().unwrap(),
                y: point_caps.get(2).unwrap().as_str().parse().unwrap(),
                z: point_caps.get(3).unwrap().as_str().parse().unwrap(),
            };
            points.insert(p);
        }
        scanners.push(Scanner {
            name: header_caps.get(1).unwrap().as_str().to_string(),
            points,
        })
    }

    scanners
}

#[derive(Debug)]
struct Scanner {
    #[allow(dead_code)]
    name: String,
    points: HashSet<Point>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
    z: i32,
}

#[cfg(test)]
mod tests {
    use crate::utils;

    use super::part1;
    use super::read_input;

    #[test]
    fn part1_example() {
        let scanners = read_input(utils::InputType::Example);
        assert_eq!(part1(&scanners), 79);
    }

    #[test]
    fn part1_real() {
        let scanners = read_input(utils::InputType::Main);
        // FIXME 429 is too high
        assert_eq!(part1(&scanners), 400);
    }
}
