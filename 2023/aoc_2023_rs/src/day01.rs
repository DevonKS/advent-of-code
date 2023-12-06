use std::collections::HashMap;

use crate::time;
use crate::util;

use aho_corasick::AhoCorasick;

pub fn run(it: util::InputType) {
    let nums = read_input(it);
    time!("Part 1", println!("{}", part1(&nums)));
    time!("Part 2", println!("{}", part2(&nums)));
}

fn part1(lines: &[String]) -> usize {
    lines
        .iter()
        .map(|s| {
            let digits: Vec<usize> = s
                .chars()
                .filter(|s| s.is_ascii_digit())
                .map(|c| c.to_digit(10).unwrap() as usize)
                .collect();
            if digits.is_empty() {
                0
            } else {
                (digits[0] * 10) + *digits.last().unwrap()
            }
        })
        .sum()
}

fn part2(lines: &[String]) -> usize {
    let patterns = [
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "1", "2", "3", "4",
        "5", "6", "7", "8", "9",
    ];

    let ac_graph = AhoCorasick::new(patterns).unwrap();

    let mut pattern_id_to_digit: HashMap<usize, usize> = HashMap::new();
    pattern_id_to_digit.insert(0, 1);
    pattern_id_to_digit.insert(1, 2);
    pattern_id_to_digit.insert(2, 3);
    pattern_id_to_digit.insert(3, 4);
    pattern_id_to_digit.insert(4, 5);
    pattern_id_to_digit.insert(5, 6);
    pattern_id_to_digit.insert(6, 7);
    pattern_id_to_digit.insert(7, 8);
    pattern_id_to_digit.insert(8, 9);
    pattern_id_to_digit.insert(9, 1);
    pattern_id_to_digit.insert(10, 2);
    pattern_id_to_digit.insert(11, 3);
    pattern_id_to_digit.insert(12, 4);
    pattern_id_to_digit.insert(13, 5);
    pattern_id_to_digit.insert(14, 6);
    pattern_id_to_digit.insert(15, 7);
    pattern_id_to_digit.insert(16, 8);
    pattern_id_to_digit.insert(17, 9);

    lines
        .iter()
        .map(|s| {
            let mut digits = vec![];
            for mat in ac_graph.find_overlapping_iter(s) {
                digits.push(pattern_id_to_digit.get(&mat.pattern().as_usize()).unwrap());
            }

            if digits.is_empty() {
                0
            } else {
                (*digits[0] * 10) + *digits.last().unwrap()
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
