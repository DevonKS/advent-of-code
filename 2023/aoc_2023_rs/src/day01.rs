use std::collections::HashMap;

use crate::time;
use crate::util;

pub fn run(it: util::InputType) {
    let nums = read_input(it);
    time!("Part 1", println!("{}", part1(&nums)));
    time!("Part 2", println!("{}", part2(&nums)));
}

fn part2(lines: &[String]) -> usize {
    let mut written_nums: HashMap<String, char> = HashMap::new();
    written_nums.insert("one".to_string(), '1');
    written_nums.insert("two".to_string(), '2');
    written_nums.insert("three".to_string(), '3');
    written_nums.insert("four".to_string(), '4');
    written_nums.insert("five".to_string(), '5');
    written_nums.insert("six".to_string(), '6');
    written_nums.insert("seven".to_string(), '7');
    written_nums.insert("eight".to_string(), '8');
    written_nums.insert("nine".to_string(), '9');

    lines
        .iter()
        .map(|s| {
            let mut current_match = String::new();
            let mut digits = Vec::new();
            for c in s.chars() {
                if c.is_ascii_digit() {
                    digits.push(c);
                    current_match.clear();
                } else {
                    current_match.push(c);
                    if let Some(n) = written_nums.get(&current_match) {
                        digits.push(*n);
                        current_match.clear();
                        current_match.push(c);
                    } else {
                        let mut is_prefix = false;
                        for (k, _) in written_nums.iter() {
                            if k.starts_with(&current_match) {
                                is_prefix = true;
                                break;
                            }
                        }

                        if !is_prefix {
                            current_match.clear();
                            current_match.push(c);
                        }
                    }
                }
            }

            if digits.is_empty() {
                0
            } else {
                [digits[0], *digits.last().unwrap()]
                    .iter()
                    .collect::<String>()
                    .parse::<usize>()
                    .unwrap()
            }
        })
        .sum()

    // FIXME: 53914 is too high
}

fn part1(lines: &[String]) -> usize {
    lines
        .iter()
        .map(|s| {
            let digits: Vec<char> = s.chars().filter(|s| s.is_ascii_digit()).collect();
            if digits.is_empty() {
                0
            } else {
                [digits[0], *digits.last().unwrap()]
                    .iter()
                    .collect::<String>()
                    .parse::<usize>()
                    .unwrap()
            }
        })
        .sum()
}

fn read_input(it: util::InputType) -> Vec<String> {
    util::parse_file(util::Day::Day01, it, |s| s.to_string())
}

#[cfg(test)]
mod tests {
    use std::fs::read_to_string;

    use crate::day01::part1;
    use crate::day01::part2;
    use crate::day01::read_input;
    use crate::util;

    #[test]
    fn part1_example() {
        let input = read_to_string("resource/day-01-example-input-pt1.txt")
            .unwrap()
            .lines()
            .map(|s| s.to_string())
            .collect::<Vec<String>>();
        assert_eq!(part1(&input), 142);
    }

    #[test]
    fn part1_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part1(&input), 54159);
    }

    #[test]
    fn part2_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part2(&input), 281);
    }

    #[test]
    fn part2_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part2(&input), 53866);
    }
}
