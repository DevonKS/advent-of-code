use std::collections::HashMap;

use regex::Regex;

use crate::{time, util};

pub fn run(it: util::InputType) {
    let games = read_input(it);
    time!("Part 1", println!("{}", part1(&games)));
    time!("Part 2", println!("{}", part2(&games)));
}

fn part1(games: &[Game]) -> usize {
    let bag = make_default_bag();
    games
        .iter()
        .filter(|g| {
            g.draws.iter().all(|d| {
                d.red_amount.unwrap_or(0) <= *bag.get(&Colour::Red).unwrap()
                    && d.green_amount.unwrap_or(0) <= *bag.get(&Colour::Green).unwrap()
                    && d.blue_amount.unwrap_or(0) <= *bag.get(&Colour::Blue).unwrap()
            })
        })
        .map(|g| g.id)
        .sum()
}

fn part2(games: &[Game]) -> usize {
    games
        .iter()
        .map(|g| {
            let min_draw = g.draws.iter().fold(
                Draw {
                    red_amount: None,
                    green_amount: None,
                    blue_amount: None,
                },
                |mut acc, d| {
                    if let Some(n) = d.red_amount {
                        if n > acc.red_amount.unwrap_or(0) {
                            acc.red_amount = Some(n);
                        }
                    };
                    if let Some(n) = d.green_amount {
                        if n > acc.green_amount.unwrap_or(0) {
                            acc.green_amount = Some(n);
                        }
                    };
                    if let Some(n) = d.blue_amount {
                        if n > acc.blue_amount.unwrap_or(0) {
                            acc.blue_amount = Some(n);
                        }
                    };
                    acc
                },
            );
            min_draw.red_amount.unwrap_or(0)
                * min_draw.green_amount.unwrap_or(0)
                * min_draw.blue_amount.unwrap_or(0)
        })
        .sum()
}

fn read_input(it: util::InputType) -> Vec<Game> {
    let game_re = Regex::new(r"^Game (\d+): ([a-z0-9 ,;]+)$").unwrap();
    let color_draw_re = Regex::new(r"^\s*(\d+) (red|green|blue)$").unwrap();
    util::parse_file(util::Day::Day02, it, |l| {
        let caps = game_re.captures(l).unwrap();
        let game_num = caps.get(1).unwrap().as_str().parse().unwrap();
        let draws_str = caps.get(2).unwrap().as_str();
        let draws = draws_str
            .split(';')
            .map(|d| {
                d.split(',').fold(
                    Draw {
                        red_amount: None,
                        green_amount: None,
                        blue_amount: None,
                    },
                    |mut acc, cd| {
                        let caps = color_draw_re.captures(cd).unwrap();
                        let amount = caps.get(1).unwrap().as_str().parse().unwrap();
                        let colour = caps.get(2).unwrap().as_str();
                        match colour {
                            "red" => acc.red_amount = Some(amount),
                            "green" => acc.green_amount = Some(amount),
                            "blue" => acc.blue_amount = Some(amount),
                            _ => panic!("unknown color: {}", colour),
                        }
                        acc
                    },
                )
            })
            .collect();
        Game {
            id: game_num,
            draws,
        }
    })
}

fn make_default_bag() -> HashMap<Colour, usize> {
    let mut bag = HashMap::new();
    bag.insert(Colour::Red, 12);
    bag.insert(Colour::Green, 13);
    bag.insert(Colour::Blue, 14);
    bag
}

struct Game {
    id: usize,
    draws: Vec<Draw>,
}

struct Draw {
    red_amount: Option<usize>,
    green_amount: Option<usize>,
    blue_amount: Option<usize>,
}

#[derive(PartialEq, Hash, Eq)]
enum Colour {
    Red,
    Green,
    Blue,
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
        assert_eq!(part1(&input), 8);
    }

    #[test]
    fn part1_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part1(&input), 2447);
    }

    #[test]
    fn part2_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part2(&input), 2286);
    }

    #[test]
    fn part2_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part2(&input), 56322);
    }
}
