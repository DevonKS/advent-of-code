use regex::Regex;

use crate::time;
use crate::utils;

pub fn run(it: utils::InputType) {
    let target_area = read_input(it);
    time!("Part 1", println!("{:?}", part1(&target_area)));
    time!("Part 2", println!("{:?}", part2(&target_area)));
}

fn part2(target_area: &TargetArea) -> i32 {
    let xs: std::ops::Range<i32> = if target_area.min_x >= 0 && target_area.max_x > 0 {
        0..target_area.max_x + 1
    } else if target_area.min_x < 0 && target_area.max_x <= 0 {
        target_area.max_x..1
    } else {
        target_area.min_x..target_area.max_x + 1
    };

    let ys: std::ops::Range<i32> = target_area.min_y..-target_area.min_y;

    let mut points: Vec<(i32, i32)> = Vec::new();
    for x in xs {
        for y in ys.clone() {
            points.push((x, y))
        }
    }

    let mut num_target_velocities = 0;
    for point in points {
        if probe_tragetory_within_target_area(target_area, point) {
            num_target_velocities += 1;
        }
    }

    num_target_velocities
}

fn part1(target_area: &TargetArea) -> i32 {
    let abs_y = target_area.min_y.abs();
    abs_y * (abs_y - 1) / 2
}

fn probe_tragetory_within_target_area(target_area: &TargetArea, point: (i32, i32)) -> bool {
    let mut point_delta = point;
    let mut current_point = point;

    while !(current_point.1 < target_area.min_y && point_delta.1 < 0) {
        if in_target_area(target_area, current_point) {
            return true;
        }

        if point_delta.0 < 0 {
            point_delta.0 += 1;
        } else if point_delta.0 > 0 {
            point_delta.0 -= 1;
        }
        point_delta.1 -= 1;

        current_point.0 += point_delta.0;
        current_point.1 += point_delta.1;
    }

    return false;
}

fn in_target_area(target_area: &TargetArea, point: (i32, i32)) -> bool {
    let (x, y) = point;
    x >= target_area.min_x
        && x <= target_area.max_x
        && y >= target_area.min_y
        && y <= target_area.max_y
}

fn read_input(it: utils::InputType) -> TargetArea {
    let input_str = utils::read_file(utils::Day::Day17, it);
    let re = Regex::new(r"^target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)$").unwrap();
    let caps = re.captures(input_str.trim()).unwrap();
    TargetArea {
        min_x: caps.get(1).unwrap().as_str().parse().unwrap(),
        max_x: caps.get(2).unwrap().as_str().parse().unwrap(),
        min_y: caps.get(3).unwrap().as_str().parse().unwrap(),
        max_y: caps.get(4).unwrap().as_str().parse().unwrap(),
    }
}

#[derive(Debug)]
struct TargetArea {
    min_x: i32,
    max_x: i32,
    min_y: i32,
    max_y: i32,
}

#[cfg(test)]
mod tests {
    use crate::day17::part1;
    use crate::day17::part2;
    use crate::day17::read_input;
    use crate::utils;

    #[test]
    fn part1_example() {
        let target_area = read_input(utils::InputType::Example);
        assert_eq!(part1(&target_area), 45);
    }

    #[test]
    fn part1_real() {
        let target_area = read_input(utils::InputType::Main);
        assert_eq!(part1(&target_area), 4278);
    }

    #[test]
    fn part2_example() {
        let target_area = read_input(utils::InputType::Example);
        assert_eq!(part2(&target_area), 112);
    }

    #[test]
    fn part2_real() {
        let target_area = read_input(utils::InputType::Main);
        assert_eq!(part2(&target_area), 1994);
    }
}
