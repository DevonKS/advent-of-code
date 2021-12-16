use crate::time;
use crate::utils;

use std::collections::HashSet;

pub fn run(it: utils::InputType) {
    let input = read_input(it);
    time!("Part 1", println!("{}", part1(&input)));
    time!("Part 2", println!("{}", part2(&input)));
}

fn part2(octs: &Vec<Vec<u8>>) -> u64 {
    let num_rows = octs.len();
    let num_cols = octs[0].len();

    let mut new_octs = octs.clone();
    let mut i: u64 = 0;
    loop {
        let num_step_flashes = run_step(&mut new_octs);
        i += 1;
        if num_step_flashes == (num_rows * num_cols) as u64 {
            break;
        }
    }
    i
}

fn part1(octs: &Vec<Vec<u8>>) -> u64 {
    let mut new_octs = octs.clone();
    let mut num_flashes: u64 = 0;
    for _i in 0..100 {
        let num_step_flashes = run_step(&mut new_octs);
        num_flashes += num_step_flashes;
    }
    num_flashes
}

fn read_input(it: utils::InputType) -> Vec<Vec<u8>> {
    utils::parse_file(utils::Day::Day11, it, |s| {
        s.chars().map(|c| c.to_digit(10).unwrap() as u8).collect()
    })
}

fn run_step(octs: &mut Vec<Vec<u8>>) -> u64 {
    let num_rows = octs.len();
    let num_cols = octs[0].len();
    let directions: [(i32, i32); 8] = [
        (-1, 0),
        (-1, 1),
        (0, 1),
        (1, 1),
        (1, 0),
        (1, -1),
        (0, -1),
        (-1, -1),
    ];

    for r in 0..num_rows {
        for c in 0..num_cols {
            octs[r][c] += 1;
        }
    }

    let mut num_flashes: u64 = 0;
    let mut flashed: HashSet<(usize, usize)> = HashSet::new();
    let mut has_flashed = true;
    while has_flashed {
        has_flashed = false;
        for r in 0..num_rows {
            for c in 0..num_cols {
                if octs[r][c] > 9 && !flashed.contains(&(r, c)) {
                    num_flashes += 1;
                    has_flashed = true;
                    flashed.insert((r, c));
                    for (rd, cd) in directions {
                        let new_r = r as i32 + rd;
                        let new_c = c as i32 + cd;
                        if 0 <= new_r
                            && (new_r as usize) < num_rows
                            && 0 <= new_c
                            && (new_c as usize) < num_cols
                        {
                            octs[new_r as usize][new_c as usize] += 1;
                        }
                    }
                }
            }
        }
    }

    for (r, c) in &flashed {
        octs[*r][*c] = 0;
    }
    num_flashes
}

#[cfg(test)]
mod tests {
    use crate::day11::part1;
    use crate::day11::part2;
    use crate::day11::read_input;
    use crate::utils;

    #[test]
    fn part1_example() {
        let octs = read_input(utils::InputType::Example);
        assert_eq!(part1(&octs), 1656);
    }

    #[test]
    fn part1_real() {
        let octs = read_input(utils::InputType::Main);
        assert_eq!(part1(&octs), 1729);
    }

    #[test]
    fn part2_example() {
        let octs = read_input(utils::InputType::Example);
        assert_eq!(part2(&octs), 195);
    }

    #[test]
    fn part2_real() {
        let octs = read_input(utils::InputType::Main);
        assert_eq!(part2(&octs), 237);
    }
}
