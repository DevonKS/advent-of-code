use regex::Regex;

use crate::{time, util};

pub fn run(it: util::InputType) {
    let instructions = read_input(it);
    time!("Part 1", println!("{}", part1(&instructions)));
    time!("Part 2", println!("{}", part2(&instructions)));
}

fn part1(instructions: &[Instruction]) -> usize {
    instructions
        .iter()
        .fold(0, |acc, inst| acc + (inst.x * inst.y))
}

fn part2(instructions: &[Instruction]) -> usize {
    instructions
        .iter()
        .filter(|inst| inst.enabled)
        .fold(0, |acc, inst| acc + (inst.x * inst.y))
}

#[derive(Debug)]
struct Instruction {
    enabled: bool,
    x: usize,
    y: usize,
}

fn read_input(it: util::InputType) -> Vec<Instruction> {
    let mut instructions = Vec::new();
    let re = Regex::new(r"mul\(([0-9]{1,3}),([0-9]{1,3})\)|(do\(\))|(don't\(\))").unwrap();
    let input = util::read_file(util::Day::Day03, it);
    let mut enabled = true;
    for c in re.captures_iter(&input) {
        if c.get(3).is_some() {
            enabled = true;
            continue;
        }

        if c.get(4).is_some() {
            enabled = false;
            continue;
        }

        instructions.push(Instruction {
            enabled,
            x: c.get(1).unwrap().as_str().parse().unwrap(),
            y: c.get(2).unwrap().as_str().parse().unwrap(),
        });
    }

    instructions
}

#[cfg(test)]
mod tests {
    use crate::day03::part1;
    use crate::day03::part2;
    use crate::day03::read_input;
    use crate::util;

    #[test]
    fn part1_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part1(&input), 161);
    }

    #[test]
    fn part1_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part1(&input), 187833789);
    }

    #[test]
    fn part2_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part2(&input), 48);
    }

    #[test]
    fn part2_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part2(&input), 94455185);
    }
}
