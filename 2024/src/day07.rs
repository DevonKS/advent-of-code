use itertools::{repeat_n, Itertools};
use regex::Regex;

use crate::{time, util};

pub fn run(it: util::InputType) {
    let equations = read_input(it);
    time!("Part 1", println!("{}", part1(&equations)));
    time!("Part 2", println!("{}", part2(&equations)));
}

fn part1(equations: &[Equation]) -> usize {
    equations
        .iter()
        .filter(|e| is_valid_equation(e))
        .map(|e| e.test_value)
        .sum()
}

fn part2(equations: &[Equation]) -> usize {
    equations
        .iter()
        .filter(|e| is_valid_equation_2(e))
        .map(|e| e.test_value)
        .sum()
}

fn is_valid_equation(equation: &Equation) -> bool {
    let ops = [Op::Add, Op::Mul];
    let op_perms = repeat_n(ops, equation.nums.len() - 1).multi_cartesian_product();

    for mut perm in op_perms {
        let res = equation
            .nums
            .clone()
            .into_iter()
            .reduce(|acc: usize, n: usize| {
                let op = perm.pop();
                match op {
                    Some(op) => match op {
                        Op::Add => acc + n,
                        Op::Mul => acc * n,
                        _ => panic!("unsupported operator {:?}", op),
                    },
                    None => panic!("incorrect number of ops"),
                }
            })
            .unwrap();

        if res == equation.test_value {
            return true;
        }
    }

    false
}

fn is_valid_equation_2(equation: &Equation) -> bool {
    let ops = [Op::Add, Op::Mul, Op::Concat];
    let op_perms = repeat_n(ops, equation.nums.len() - 1).multi_cartesian_product();

    for mut perm in op_perms {
        let res = equation
            .nums
            .clone()
            .into_iter()
            .reduce(|acc: usize, n: usize| {
                let op = perm.pop();
                match op {
                    Some(op) => match op {
                        Op::Add => acc + n,
                        Op::Mul => acc * n,
                        Op::Concat => {
                            let mut new_num_str = acc.to_string();
                            new_num_str.push_str(&n.to_string());
                            new_num_str.parse().unwrap()
                        }
                    },
                    None => panic!("incorrect number of ops"),
                }
            })
            .unwrap();

        if res == equation.test_value {
            return true;
        }
    }

    false
}

#[derive(Copy, Clone, Debug)]
enum Op {
    Add,
    Mul,
    Concat,
}

struct Equation {
    test_value: usize,
    nums: Vec<usize>,
}

fn read_input(it: util::InputType) -> Vec<Equation> {
    util::parse_file(util::Day::Day07, it, |l| {
        let re = Regex::new(r"^(\d+): (.+)$").unwrap();
        let caps = re.captures(l).unwrap();
        let nums = caps
            .get(2)
            .unwrap()
            .as_str()
            .split(" ")
            .map(|s| s.parse().unwrap())
            .collect();
        Equation {
            test_value: caps.get(1).unwrap().as_str().parse().unwrap(),
            nums,
        }
    })
}

#[cfg(test)]
mod tests {
    use crate::day07::part1;
    use crate::day07::part2;
    use crate::day07::read_input;
    use crate::util;

    #[test]
    fn part1_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part1(&input), 3749);
    }

    #[test]
    fn part1_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part1(&input), 28730327770375);
    }

    #[test]
    fn part2_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part2(&input), 11387);
    }

    #[test]
    #[ignore]
    fn part2_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part2(&input), 424977609625985);
    }
}
