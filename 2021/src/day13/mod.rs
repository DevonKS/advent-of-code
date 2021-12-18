use crate::time;
use crate::utils;

use regex::Regex;
use std::collections::HashSet;

pub fn run(it: utils::InputType) {
    let input = read_input(it);
    time!("Part 1", println!("{}", part1(&input)));
    time!("Part 2", println!("{}", part2(&input)));
}

fn part2(input: &Input) -> String {
    let mut folded_points: HashSet<(i32, i32)> = HashSet::new();
    for point in &input.points {
        let mut folded_point = point.clone();
        for fold in &input.folds {
            folded_point = fold_point(&folded_point, fold)
        }

        folded_points.insert(folded_point);
    }

    let max_x: i32 = folded_points.iter().map(|(x, _y)| *x).max().unwrap();
    let min_x: i32 = folded_points.iter().map(|(x, _y)| *x).min().unwrap();
    let max_y: i32 = folded_points.iter().map(|(_x, y)| *y).max().unwrap();
    let min_y: i32 = folded_points.iter().map(|(_x, y)| *y).min().unwrap();

    let mut output = String::new();
    for y in min_y..max_y + 1 {
        for x in min_x..max_x + 1 {
            output.push_str(if folded_points.contains(&(x, y)) {
                "#"
            } else {
                " "
            });
        }
        output.push_str("\n");
    }
    output
}

fn part1(input: &Input) -> u64 {
    let mut folded_points: HashSet<(i32, i32)> = HashSet::new();
    input.points.iter().for_each(|p| {
        folded_points.insert(fold_point(p, &input.folds[0]));
    });
    folded_points.len().try_into().unwrap()
}

fn fold_point(point: &(i32, i32), fold: &Fold) -> (i32, i32) {
    match fold.axis {
        Axis::Y => {
            if point.1 < fold.n as i32 {
                return *point;
            }

            let (x, y) = point;
            let y_diff = y - fold.n as i32;
            let new_y = fold.n as i32 - y_diff;
            (*x, new_y)
        }
        Axis::X => {
            if point.0 < fold.n as i32 {
                return *point;
            }

            let (x, y) = point;
            let x_diff = x - fold.n as i32;
            let new_x = fold.n as i32 - x_diff;
            (new_x, *y)
        }
    }
}

fn read_input(it: utils::InputType) -> Input {
    let input_str = utils::read_file(utils::Day::Day13, it);
    let parts: Vec<&str> = input_str.split("\n\n").collect();
    assert_eq!(2, parts.len());
    let points: Vec<(i32, i32)> = parts[0]
        .lines()
        .map(|s| {
            let parts: Vec<&str> = s.split(",").collect();
            assert_eq!(2, parts.len());
            (parts[0].parse().unwrap(), parts[1].parse().unwrap())
        })
        .collect();
    let folds: Vec<Fold> = parts[1]
        .lines()
        .map(|s| {
            let re = Regex::new(r"^fold along ([x|y])=(\d+)$").unwrap();
            let caps = re.captures(s).unwrap();
            Fold {
                axis: match caps.get(1).unwrap().as_str() {
                    "y" => Axis::Y,
                    "x" => Axis::X,
                    _ => panic!("Unknown axis"),
                },
                n: caps.get(2).unwrap().as_str().parse().unwrap(),
            }
        })
        .collect();
    Input { points, folds }
}

struct Input {
    points: Vec<(i32, i32)>,
    folds: Vec<Fold>,
}

struct Fold {
    axis: Axis,
    n: u32,
}

enum Axis {
    Y,
    X,
}

#[cfg(test)]
mod tests {
    use crate::day13::part1;
    use crate::day13::part2;
    use crate::day13::read_input;
    use crate::utils;

    #[test]
    fn part1_example() {
        let input = read_input(utils::InputType::Example);
        assert_eq!(part1(&input), 17);
    }

    #[test]
    fn part1_real() {
        let input = read_input(utils::InputType::Main);
        assert_eq!(part1(&input), 716);
    }

    #[test]
    fn part2_example() {
        let input = read_input(utils::InputType::Example);
        assert_eq!(
            part2(&input),
            "#####
#   #
#   #
#   #
#####
"
        );
    }

    #[test]
    fn part2_real() {
        let input = read_input(utils::InputType::Main);
        assert_eq!(
            part2(&input),
            "###  ###   ##  #  # #### ###  #    ### \n#  # #  # #  # # #  #    #  # #    #  #\n#  # #  # #    ##   ###  ###  #    #  #\n###  ###  #    # #  #    #  # #    ### \n# #  #    #  # # #  #    #  # #    # # \n#  # #     ##  #  # #    ###  #### #  #\n"
        );
    }
}
