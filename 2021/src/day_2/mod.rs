use crate::utils;

use regex::Regex;

pub fn run() {
    let input = read_input(utils::InputType::Main);
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}

fn part2(insts: &Vec<Instruction>) -> i32 {
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

fn part1(insts: &Vec<Instruction>) -> i32 {
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

fn read_input(it: utils::InputType) -> Vec<Instruction> {
    utils::parse_file(utils::Day::Day02, it, |s| {
        let re = Regex::new(r"^(forward|down|up) (\d+)$").unwrap();
        let caps = re.captures(s).unwrap();
        let inst = Instruction {
            command: match caps.get(1).unwrap().as_str() {
                "forward" => Command::Forward,
                "down" => Command::Down,
                "up" => Command::Up,
                _ => panic!("Unknown command"),
            },
            size: caps.get(2).unwrap().as_str().parse().unwrap(),
        };
        inst
    })
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

mod tests {
    // FIXME Why am I getting a unsed import warning here?
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn challenge_1_example() {
        let insts = read_input(utils::InputType::Example);
        assert_eq!(part1(&insts), 150);
    }

    #[test]
    fn challenge_1_real() {
        let insts = read_input(utils::InputType::Main);
        assert_eq!(part1(&insts), 1882980);
    }

    #[test]
    fn challenge_2_example() {
        let insts = read_input(utils::InputType::Example);
        assert_eq!(part2(&insts), 900);
    }

    #[test]
    fn challenge_2_real() {
        let insts = read_input(utils::InputType::Main);
        assert_eq!(part2(&insts), 1971232560);
    }
}
