use std::fmt;
use std::fs::read_to_string;

pub fn main() {
    let game = read_input("resources/day-04-input.txt");
    println!("{}", challenge_1(&game));
    println!("{}", challenge_2(&game));
}

fn challenge_2(game: &BingoGame) -> i32 {
    let mut result: i32 = 0;
    let mut local_game = game.clone();
    'num_loop: for num in local_game.nums {
        let last_board_to_win = local_game.boards.iter().filter(|b| !b.has_won).count() == 1;
        for board in local_game.boards.iter_mut() {
            if !board.has_won {
                for row in &mut *board.board {
                    for mut cell in row {
                        if !cell.marked && cell.n == num {
                            cell.marked = true
                        }
                    }
                }

                board.has_won = is_win(&board.board);

                if board.has_won && last_board_to_win {
                    let unmarked = get_unmarked(&board.board);
                    let unmarked_sum: i32 = unmarked.iter().sum();
                    result = num * unmarked_sum;
                    break 'num_loop;
                }
            }
        }
    }
    result
}

fn challenge_1(game: &BingoGame) -> i32 {
    let mut result: i32 = 0;
    let mut local_game = game.clone();
    'num_loop: for num in local_game.nums {
        for board in local_game.boards.iter_mut() {
            for row in &mut *board.board {
                for mut cell in row {
                    if !cell.marked && cell.n == num {
                        cell.marked = true
                    }
                }
            }
            if is_win(&board.board) {
                let unmarked = get_unmarked(&board.board);
                let unmarked_sum: i32 = unmarked.iter().sum();
                result = num * unmarked_sum;
                break 'num_loop;
            }
        }
    }
    result
}

fn is_win(board: &Vec<Vec<BingoCell>>) -> bool {
    let mut col_range = 0..board[0].len();

    board.iter().any(|row| row.iter().all(|c| c.marked))
        || col_range.any(|i| {
            let mut res = true;
            for row in board {
                if !row[i].marked {
                    res = false;
                    break;
                }
            }
            res
        })
}

fn get_unmarked(board: &Vec<Vec<BingoCell>>) -> Vec<i32> {
    let mut v: Vec<i32> = Vec::new();
    for row in board {
        for cell in row {
            if !cell.marked {
                v.push(cell.n);
            }
        }
    }
    v
}

fn read_input(filepath: &str) -> BingoGame {
    let xs: Vec<String> = read_to_string(filepath)
        .unwrap()
        .trim()
        .split("\n\n")
        .map(|s| s.to_string())
        .collect();

    let nums: Vec<i32> = xs[0].split(",").map(|s| s.parse().unwrap()).collect();
    let mut boards: Vec<BingoBoard> = Vec::new();
    for string_board in &xs[1..] {
        let mut board: Vec<Vec<BingoCell>> = Vec::new();
        for row_string in string_board.split("\n") {
            let row: Vec<BingoCell> = row_string
                .split_whitespace()
                .map(|s| BingoCell {
                    n: s.parse().unwrap(),
                    marked: false,
                })
                .collect();
            board.push(row);
        }
        let bingo_board = BingoBoard {
            board,
            has_won: false,
        };
        boards.push(bingo_board);
    }
    let bingo_game = BingoGame { nums, boards };
    bingo_game
}

struct BingoGame {
    nums: Vec<i32>,
    boards: Vec<BingoBoard>,
}

impl Clone for BingoGame {
    fn clone(&self) -> BingoGame {
        BingoGame {
            nums: self.nums.clone(),
            boards: self.boards.clone(),
        }
    }
}

struct BingoBoard {
    has_won: bool,
    board: Vec<Vec<BingoCell>>,
}

impl Clone for BingoBoard {
    fn clone(&self) -> BingoBoard {
        BingoBoard {
            has_won: self.has_won,
            board: self.board.clone(),
        }
    }
}

struct BingoCell {
    n: i32,
    marked: bool,
}

impl Clone for BingoCell {
    fn clone(&self) -> BingoCell {
        BingoCell {
            n: self.n,
            marked: self.marked,
        }
    }
}

impl fmt::Debug for BingoCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BingoCell")
            .field("n", &self.n)
            .field("marked", &self.marked)
            .finish()
    }
}

mod tests {
    // FIXME Why am I getting a unsed import warning here?
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn challenge_1_example() {
        let game = read_input("resources/day-04-test-input.txt");
        assert_eq!(challenge_1(&game), 4512);
    }

    #[test]
    fn challenge_1_real() {
        let game = read_input("resources/day-04-input.txt");
        assert_eq!(challenge_1(&game), 69579);
    }

    #[test]
    fn challenge_2_example() {
        let game = read_input("resources/day-04-test-input.txt");
        assert_eq!(challenge_2(&game), 1924);
    }

    #[test]
    fn challenge_2_real() {
        let game = read_input("resources/day-04-input.txt");
        assert_eq!(challenge_2(&game), 14877);
    }
}
