use crate::{time, util};

pub fn run(it: util::InputType) {
    let rotations = read_input(it);
    time!("Part 1", println!("{}", part1(&rotations)));
    time!("Part 2", println!("{}", part2(&rotations)));
}

#[derive(Debug)]
enum Direction {
    L,
    R,
}

impl TryFrom<&str> for Direction {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "L" => Ok(Direction::L),
            "R" => Ok(Direction::R),
            _ => Err(format!("failed to parse direction {}", value)),
        }
    }
}

struct Rotation {
    direction: Direction,
    clicks: u64,
}

fn part1(rotations: &[Rotation]) -> usize {
    let mut zeros = 0;
    let mut current_pos: i64 = 50;

    for r in rotations.iter() {
        current_pos = match r.direction {
            Direction::L => (current_pos - r.clicks as i64) % 100,
            Direction::R => (current_pos + r.clicks as i64) % 100,
        };

        if current_pos < 0 {
            current_pos += 100;
        }

        if current_pos == 0 {
            zeros += 1;
        }
    }

    zeros
}

fn part2(rotations: &[Rotation]) -> usize {
    let mut zeros = 0;
    let mut current_pos: i64 = 50;

    for r in rotations.iter() {
        match r.direction {
            Direction::L => {
                if current_pos == 0 {
                    zeros += (current_pos + r.clicks as i64) / 100;
                } else {
                    zeros += (100 - current_pos + r.clicks as i64) / 100;
                }
                current_pos = (current_pos - r.clicks as i64) % 100;
            }
            Direction::R => {
                zeros += (current_pos + r.clicks as i64) / 100;
                current_pos = (current_pos + r.clicks as i64) % 100;
            }
        };

        if current_pos < 0 {
            current_pos += 100;
        }
    }

    zeros as usize
}

fn read_input(it: util::InputType) -> Vec<Rotation> {
    util::parse_file(util::Day::Day01, it, |line| {
        let (direction_str, clicks_str) = line.split_at(1);
        Rotation {
            direction: Direction::try_from(direction_str).expect("unable to parse direction"),
            clicks: clicks_str.parse().expect("unable to parse clicks"),
        }
    })
}

#[cfg(test)]
mod tests {
    use crate::day01::part1;
    use crate::day01::part2;
    use crate::day01::read_input;
    use crate::util;

    #[test]
    fn part1_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part1(&input), 3);
    }

    #[test]
    fn part1_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part1(&input), 1150);
    }

    #[test]
    fn part2_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part2(&input), 6);
    }

    #[test]
    fn part2_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part2(&input), 6738);
    }
}
