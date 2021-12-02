use std::error::Error;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

pub fn main() {
    let input_result = read_input("resources/day-01-input.txt");
    match input_result {
        Ok(input) => {
            println!("{}", challenge_1_v2(&input));
            println!("{}", challenge_2_v2(&input));
        }
        Err(err) => println!("Error reading input: {}", err),
    }
}

fn challenge_2(nums: &Vec<u32>) -> u32 {
    let mut res = 0;
    for x in 3..nums.len() {
        let a1 = nums[x - 3];
        let a2 = nums[x - 2];
        let a3 = nums[x - 1];
        let b1 = a2;
        let b2 = a3;
        let b3 = nums[x];
        let sum_a = a1 + a2 + a3;
        let sum_b = b1 + b2 + b3;
        if sum_b > sum_a {
            res += 1
        }
    }
    res
}

fn challenge_2_v2(nums: &Vec<u32>) -> u32 {
    nums.windows(4)
        .filter(|x| x[3] > x[0])
        .count()
        .try_into()
        .unwrap()
}

fn challenge_1(nums: &Vec<u32>) -> u32 {
    let mut res = 0;
    for x in 1..nums.len() {
        let previous_num = nums[x - 1];
        let num = nums[x];
        if num > previous_num {
            res += 1
        }
    }
    res
}

fn challenge_1_v2(nums: &Vec<u32>) -> u32 {
    nums.windows(2)
        .filter(|x| x[1] > x[0])
        .count()
        .try_into()
        .unwrap()
}

fn read_input(filepath: &str) -> Result<Vec<u32>, Box<dyn Error>> {
    let lines = read_lines(filepath)?;
    let mut v: Vec<u32> = Vec::new();
    for line_result in lines {
        let line = line_result?;
        let num = line.parse::<u32>()?;
        v.push(num)
    }
    Ok(v)
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
    use super::*;

    #[test]
    fn challenge_1_example() {
        match read_input("resources/day-01-test-input.txt") {
            Ok(input) => {
                assert_eq!(challenge_1(&input), 7);
                assert_eq!(challenge_1_v2(&input), 7);
            }
            Err(err) => panic!("Failed to read input: {}", err),
        }
    }

    #[test]
    fn challenge_2_example() {
        match read_input("resources/day-01-test-input.txt") {
            Ok(input) => {
                assert_eq!(challenge_2(&input), 5);
                assert_eq!(challenge_2_v2(&input), 5);
            }
            Err(err) => panic!("Failed to read input: {}", err),
        }
    }

    #[test]
    fn challenge_1_real() {
        match read_input("resources/day-01-input.txt") {
            Ok(input) => {
                assert_eq!(challenge_1(&input), 1581);
                assert_eq!(challenge_1_v2(&input), 1581);
            }
            Err(err) => panic!("Failed to read input: {}", err),
        }
    }

    #[test]
    fn challenge_2_real() {
        match read_input("resources/day-01-input.txt") {
            Ok(input) => {
                assert_eq!(challenge_2(&input), 1618);
                assert_eq!(challenge_2_v2(&input), 1618);
            }
            Err(err) => panic!("Failed to read input: {}", err),
        }
    }
}
