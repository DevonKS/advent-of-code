use regex::Regex;

use crate::{time, utils};

pub fn run(it: utils::InputType) {
    let board = read_input(it);
    time!("Part 1", println!("{}", part1(&mut board.clone())));
}

fn part1(board: &mut Board) -> usize {
    let mut dice = DeterministicDice::new();
    let winner = play_game(board, &mut dice);
    match winner {
        Player::Player1 => board.p2_score * dice.times_rolled,
        Player::Player2 => board.p1_score * dice.times_rolled,
    }
}

fn play_game(board: &mut Board, dice: &mut DeterministicDice) -> Player {
    let mut current_player = Player::Player1;
    let winner: Option<Player>;
    loop {
        board.make_move(current_player, dice);
        let w = board.get_winner();
        if w.is_some() {
            winner = w;
            break;
        }

        current_player = match current_player {
            Player::Player1 => Player::Player2,
            Player::Player2 => Player::Player1,
        }
    }
    winner.unwrap()
}

fn read_input(it: utils::InputType) -> Board {
    let re = Regex::new(r"^Player (\d) starting position: (\d)$").unwrap();
    let input_str = utils::read_file(utils::Day::Day21, it);
    let mut p1_pos = 0;
    let mut p2_pos = 0;
    for l in input_str.lines() {
        let caps = re.captures(l).unwrap();
        let player_num = caps.get(1).unwrap().as_str().parse().unwrap();
        match player_num {
            1 => {
                p1_pos = caps.get(2).unwrap().as_str().parse().unwrap();
            }
            2 => {
                p2_pos = caps.get(2).unwrap().as_str().parse().unwrap();
            }
            _ => {
                panic!("Unknown player: {}", player_num);
            }
        }
    }
    Board::new(p1_pos, p2_pos)
}

#[derive(Debug, Clone)]
struct Board {
    p1_pos: usize,
    p1_score: usize,
    p2_pos: usize,
    p2_score: usize,
}

impl Board {
    fn new(p1_pos: usize, p2_pos: usize) -> Board {
        Board {
            p1_pos: p1_pos - 1,
            p1_score: 0,
            p2_pos: p2_pos - 1,
            p2_score: 0,
        }
    }

    fn make_move(&mut self, p: Player, dice: &mut DeterministicDice) {
        let addition = dice.roll() + dice.roll() + dice.roll();
        match p {
            Player::Player1 => {
                self.p1_pos = (self.p1_pos + addition) % 10;
                self.p1_score += self.p1_pos + 1;
            }
            Player::Player2 => {
                self.p2_pos = (self.p2_pos + addition) % 10;
                self.p2_score += self.p2_pos + 1;
            }
        }
    }

    fn get_winner(&self) -> Option<Player> {
        if self.p1_score >= 1000 {
            return Some(Player::Player1);
        } else if self.p2_score >= 1000 {
            return Some(Player::Player2);
        } else {
            return None;
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Player {
    Player1,
    Player2,
}

#[derive(Debug)]
struct DeterministicDice {
    i: usize,
    times_rolled: usize,
}

impl DeterministicDice {
    fn new() -> DeterministicDice {
        DeterministicDice {
            i: 0,
            times_rolled: 0,
        }
    }

    fn roll(&mut self) -> usize {
        self.times_rolled += 1;
        let res = self.i;
        self.i = (self.i + 1) % 100;
        res + 1
    }
}

#[cfg(test)]
mod tests {
    use crate::utils;

    use super::part1;
    use super::read_input;

    #[test]
    fn part1_example() {
        let board = read_input(utils::InputType::Example);
        assert_eq!(part1(&mut board.clone()), 739785)
    }

    #[test]
    fn part1_real() {
        let board = read_input(utils::InputType::Main);
        assert_eq!(part1(&mut board.clone()), 734820)
    }
}
