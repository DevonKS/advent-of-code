use crate::{time, util};

pub fn run(it: util::InputType) {
    let id_ranges = read_input(it);
    time!("Part 1", println!("{}", part1(&id_ranges)));
    time!("Part 2", println!("{}", part2(&id_ranges)));
}

fn part1(id_ranges: &[IdRange]) -> usize {
    let mut invalid_ids = Vec::new();

    for r in id_ranges.iter() {
        for i in r.lower..=r.upper {
            let s = i.to_string();
            if s.len() % 2 == 0 {
                let half_index = s.len() / 2;
                let (first, second) = s.split_at(half_index);
                if first == second {
                    invalid_ids.push(i);
                }
            }
        }
    }

    invalid_ids.iter().sum()
}

fn part2(id_ranges: &[IdRange]) -> usize {
    let mut invalid_ids = Vec::new();

    for r in id_ranges.iter() {
        for i in r.lower..=r.upper {
            let s = i.to_string();
            let half_index = s.len() / 2;
            for seq_len in 1..=half_index {
                let c = s
                    .as_bytes()
                    .chunks(seq_len)
                    .map(str::from_utf8)
                    .collect::<Result<Vec<&str>, _>>()
                    .expect("failed to chunk number");
                if c.iter().all(|&i| i == c[0]) {
                    invalid_ids.push(i);
                    break;
                }
            }
        }
    }

    invalid_ids.iter().sum()
}

struct IdRange {
    lower: usize,
    upper: usize,
}

fn read_input(it: util::InputType) -> Vec<IdRange> {
    let input_str = util::read_file(util::Day::Day02, it);

    input_str
        .trim()
        .split(',')
        .map(|id_range| {
            let parts: Vec<&str> = id_range.split('-').collect();
            if parts.len() != 2 {
                panic!("expected 2 parts in id range: {}", id_range);
            }

            IdRange {
                lower: parts[0].parse().expect("failed to parse lower in id range"),
                upper: parts[1].parse().expect("failed to parse upper in id range"),
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::day02::part1;
    use crate::day02::part2;
    use crate::day02::read_input;
    use crate::util;

    #[test]
    fn part1_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part1(&input), 1227775554);
    }

    #[test]
    fn part1_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part1(&input), 12599655151);
    }

    #[test]
    fn part2_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part2(&input), 4174379265);
    }

    #[test]
    fn part2_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part2(&input), 20942028255);
    }
}
