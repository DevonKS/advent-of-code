use regex::Regex;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

pub fn main() {
    let input = read_input("resources/day-02-input.txt");
    println!("{}", challenge_1(&input));
    println!("{}", challenge_2(&input));
}

fn challenge_2(insts: &Vec<Instruction>) -> i32 {
    let mut pos = Position {
        horizontal_pos: 0,
        depth: 0,
        aim: 0,
    };
    for inst in insts {
        match inst.command {
            Command::Forward => {
                pos.horizontal_pos += inst.size as i32;
                pos.depth += (inst.size as i32) * pos.aim;
            }
            Command::Down => pos.aim += inst.size as i32,
            Command::Up => pos.aim -= inst.size as i32,
        }
    }
    pos.horizontal_pos * pos.depth
}

fn challenge_1(insts: &Vec<Instruction>) -> i32 {
    let mut pos = Position {
        horizontal_pos: 0,
        depth: 0,
        aim: 0,
    };
    for inst in insts {
        match inst.command {
            Command::Forward => pos.horizontal_pos += inst.size as i32,
            Command::Down => pos.depth += inst.size as i32,
            Command::Up => pos.depth -= inst.size as i32,
        }
    }
    pos.horizontal_pos * pos.depth
}

fn read_input(filepath: &str) -> Vec<Instruction> {
    let lines = read_lines(filepath).unwrap();
    let mut v: Vec<Instruction> = Vec::new();
    for line_result in lines {
        let line = line_result.unwrap();
        let re = Regex::new(r"^(forward|down|up) (\d+)$").unwrap();
        let caps = re.captures(&line).unwrap();
        let inst = Instruction {
            command: match caps.get(1).unwrap().as_str() {
                "forward" => Command::Forward,
                "down" => Command::Down,
                "up" => Command::Up,
                _ => panic!("Unknown command"),
            },
            size: caps.get(2).unwrap().as_str().parse().unwrap(),
        };
        v.push(inst)
    }
    v
}

struct Position {
    horizontal_pos: i32,
    depth: i32,
    aim: i32,
}

enum Command {
    Forward,
    Down,
    Up,
}

struct Instruction {
    command: Command,
    size: u32,
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
        let insts = read_input("resources/day-02-test-input.txt");
        assert_eq!(challenge_1(&insts), 150);
    }

    #[test]
    fn challenge_1_real() {
        let insts = read_input("resources/day-02-input.txt");
        assert_eq!(challenge_1(&insts), 1882980);
    }

    #[test]
    fn challenge_2_example() {
        let insts = read_input("resources/day-02-test-input.txt");
        assert_eq!(challenge_2(&insts), 900);
    }

    #[test]
    fn challenge_2_real() {
        let insts = read_input("resources/day-02-input.txt");
        assert_eq!(challenge_2(&insts), 1971232560);
    }
}
