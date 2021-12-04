use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

pub fn main() {
    let input = read_input("resources/day-03-input.txt");
    println!("{}", challenge_1(&input));
    println!("{}", challenge_2(&input));
}

fn challenge_2(binary_nums: &Vec<Vec<char>>) -> i32 {
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

fn challenge_1(binary_nums: &Vec<Vec<char>>) -> i32 {
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

fn read_input(filepath: &str) -> Vec<Vec<char>> {
    let lines = read_lines(filepath).unwrap();
    let mut v: Vec<Vec<char>> = Vec::new();
    for line_result in lines {
        let line = line_result.unwrap().chars().collect();
        v.push(line)
    }
    v
}

// FIXME Move this into a utility
// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

mod tests {
    // FIXME Why am I getting a unsed import warning here?
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn challenge_1_example() {
        let binary_nums = read_input("resources/day-03-test-input.txt");
        assert_eq!(challenge_1(&binary_nums), 198);
    }

    #[test]
    fn challenge_1_real() {
        let binary_nums = read_input("resources/day-03-input.txt");
        assert_eq!(challenge_1(&binary_nums), 1131506);
    }

    #[test]
    fn challenge_2_example() {
        let binary_nums = read_input("resources/day-03-test-input.txt");
        assert_eq!(challenge_2(&binary_nums), 230);
    }

    #[test]
    fn challenge_2_real() {
        let binary_nums = read_input("resources/day-03-input.txt");
        assert_eq!(challenge_2(&binary_nums), 7863147);
    }
}
