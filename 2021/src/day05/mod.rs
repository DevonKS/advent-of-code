use crate::time;
use crate::utils;

use regex::Regex;
use std::collections::HashMap;

pub fn run(it: utils::InputType) {
    let input = read_input(it);
    time!("Part 1", println!("{}", part1(&input)));
    time!("Part 2", println!("{}", part2(&input)));
}

fn part2(lines: &Vec<Line>) -> i32 {
    let all_points: Vec<Point> = lines.iter().flat_map(|l| points_on_line(l)).collect();

    let mut quantities: HashMap<Point, u32> = HashMap::new();
    for point in all_points {
        if quantities.contains_key(&point) {
            *quantities.get_mut(&point).unwrap() += 1;
        } else {
            quantities.insert(point, 1);
        }
    }
    quantities
        .iter()
        .filter(|&(_k, v)| *v > 1)
        .count()
        .try_into()
        .unwrap()
}

fn part1(lines: &Vec<Line>) -> i32 {
    let all_points: Vec<Point> = lines
        .iter()
        .filter(|l| l.start.x == l.end.x || l.start.y == l.end.y)
        .flat_map(|l| points_on_line(l))
        .collect();

    let mut quantities: HashMap<Point, u32> = HashMap::new();
    for point in all_points {
        if quantities.contains_key(&point) {
            *quantities.get_mut(&point).unwrap() += 1;
        } else {
            quantities.insert(point, 1);
        }
    }
    quantities
        .iter()
        .filter(|&(_k, v)| *v > 1)
        .count()
        .try_into()
        .unwrap()
}

fn points_on_line(l: &Line) -> Vec<Point> {
    let mut x_delta = 0;
    let mut y_delta = 0;
    if l.start.y == l.end.y {
        // Horizontal Line
        x_delta = if l.start.x > l.end.x { -1 } else { 1 };
    } else if l.start.x == l.end.x {
        // Vertical Line
        y_delta = if l.start.y > l.end.y { -1 } else { 1 };
    } else {
        let gradient = ((l.end.y - l.start.y) / (l.end.x - l.start.x)).abs();
        y_delta = if l.start.y > l.end.y {
            -gradient
        } else {
            gradient
        };
        x_delta = if l.start.x > l.end.x { -1 } else { 1 };
    }

    let mut v: Vec<Point> = Vec::new();
    v.push(l.start.clone());
    let mut current_point = l.start.clone();
    let mut count = 0;
    while current_point != l.end {
        let new_point = Point {
            x: current_point.x + x_delta,
            y: current_point.y + y_delta,
        };
        current_point = new_point.clone();
        v.push(new_point);

        if count > 1000 {
            break;
        }
        count += 1;
    }
    v
}

fn read_input(it: utils::InputType) -> Vec<Line> {
    utils::parse_file(utils::Day::Day05, it, |s| {
        let re = Regex::new(r"^(\d+),(\d+) -> (\d+),(\d+)$").unwrap();
        let caps = re.captures(s).unwrap();
        let line = Line {
            start: Point {
                x: caps.get(1).unwrap().as_str().parse().unwrap(),
                y: caps.get(2).unwrap().as_str().parse().unwrap(),
            },
            end: Point {
                x: caps.get(3).unwrap().as_str().parse().unwrap(),
                y: caps.get(4).unwrap().as_str().parse().unwrap(),
            },
        };
        line
    })
}

struct Line {
    start: Point,
    end: Point,
}

#[derive(Eq, PartialEq, PartialOrd, Ord, Hash, Clone, Debug)]
struct Point {
    x: i32,
    y: i32,
}

#[cfg(test)]
mod tests {
    use crate::day05::part1;
    use crate::day05::part2;
    use crate::day05::read_input;
    use crate::utils;

    #[test]
    fn part1_example() {
        let lines = read_input(utils::InputType::Example);
        assert_eq!(part1(&lines), 5);
    }

    #[test]
    fn part1_real() {
        let lines = read_input(utils::InputType::Main);
        assert_eq!(part1(&lines), 5576);
    }

    #[test]
    fn part2_example() {
        let lines = read_input(utils::InputType::Example);
        assert_eq!(part2(&lines), 12);
    }

    #[test]
    fn part2_real() {
        let lines = read_input(utils::InputType::Main);
        assert_eq!(part2(&lines), 18144);
    }
}
