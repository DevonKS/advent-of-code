use crate::time;
use crate::utils;

pub fn run(it: utils::InputType) {
    let input = read_input(it);
    time!("Part 1", println!("{}", part1(&input)));
    time!("Part 2", println!("{}", part2(&input)));
}

fn part2(binary_nums: &Vec<Vec<char>>) -> i32 {
    find_number(binary_nums, 0, true) * find_number(binary_nums, 0, false)
}

fn find_number(binary_nums: &Vec<Vec<char>>, i: usize, ones: bool) -> i32 {
    if binary_nums.len() <= 1 {
        let s: String = binary_nums[0].iter().collect();
        return i32::from_str_radix(&s, 2).unwrap();
    } else {
        let filter_num = bit_criteria(binary_nums, i, ones);
        let new_nums: Vec<Vec<char>> = binary_nums
            .into_iter()
            .filter(|s| s[i] == filter_num)
            .map(|s| s.clone())
            .collect();
        return find_number(&new_nums, i + 1, ones);
    }
}

fn bit_criteria(binary_nums: &Vec<Vec<char>>, i: usize, most: bool) -> char {
    let mut num1s = 0;
    let mut num0s = 0;
    for binary_num in binary_nums {
        if '1' == binary_num[i] {
            num1s += 1;
        } else {
            num0s += 1;
        }
    }
    if most {
        return if num1s >= num0s { '1' } else { '0' };
    } else {
        return if num1s >= num0s { '0' } else { '1' };
    }
}

fn part1(binary_nums: &Vec<Vec<char>>) -> i32 {
    let mut gamma = String::new();
    let mut epsilon = String::new();
    for i in 0..binary_nums[0].len() {
        let mut num1s = 0;
        let mut num0s = 0;
        for binary_num in binary_nums {
            if '1' == binary_num[i] {
                num1s += 1;
            } else {
                num0s += 1;
            }
        }

        if num1s > num0s {
            gamma.push('1');
            epsilon.push('0');
        } else {
            gamma.push('0');
            epsilon.push('1');
        }
    }
    i32::from_str_radix(&gamma, 2).unwrap() * i32::from_str_radix(&epsilon, 2).unwrap()
}

fn read_input(it: utils::InputType) -> Vec<Vec<char>> {
    utils::parse_file(utils::Day::Day03, it, |s| s.chars().collect())
}

#[cfg(test)]
mod tests {
    use crate::day03::part1;
    use crate::day03::part2;
    use crate::day03::read_input;
    use crate::utils;

    #[test]
    fn part1_example() {
        let binary_nums = read_input(utils::InputType::Example);
        assert_eq!(part1(&binary_nums), 198);
    }

    #[test]
    fn part1_real() {
        let binary_nums = read_input(utils::InputType::Main);
        assert_eq!(part1(&binary_nums), 1131506);
    }

    #[test]
    fn part2_example() {
        let binary_nums = read_input(utils::InputType::Example);
        assert_eq!(part2(&binary_nums), 230);
    }

    #[test]
    fn part2_real() {
        let binary_nums = read_input(utils::InputType::Main);
        assert_eq!(part2(&binary_nums), 7863147);
    }
}
