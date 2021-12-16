use crate::time;
use crate::utils;

pub fn run(it: utils::InputType) {
    let game = read_input(it);
    time!("Part 1", println!("{}", part1(&game)));
    time!("Part 2", println!("{}", part2(&game)));
}

fn part2(game: &BingoGame) -> i32 {
    let mut result: i32 = 0;
    let mut local_game = game.clone();
    'num_loop: for num in local_game.nums {
        let last_board_to_win = local_game.boards.iter().filter(|b| !b.has_won).count() == 1;
        for board in local_game.boards.iter_mut() {
            if !board.has_won {
                for row in board.board.iter_mut() {
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

fn part1(game: &BingoGame) -> i32 {
    let mut result: i32 = 0;
    let mut local_game = game.clone();
    'num_loop: for num in local_game.nums {
        for board in local_game.boards.iter_mut() {
            for row in board.board.iter_mut() {
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
        || col_range.any(|i| board.iter().all(|r| r[i].marked))
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

fn read_input(it: utils::InputType) -> BingoGame {
    let xs: Vec<String> = utils::read_file(utils::Day::Day04, it)
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

#[derive(Debug, Clone)]
struct BingoGame {
    nums: Vec<i32>,
    boards: Vec<BingoBoard>,
}

#[derive(Debug, Clone)]
struct BingoBoard {
    has_won: bool,
    board: Vec<Vec<BingoCell>>,
}

#[derive(Debug, Clone)]
struct BingoCell {
    n: i32,
    marked: bool,
}

#[cfg(test)]
mod tests {
    use crate::day04::part1;
    use crate::day04::part2;
    use crate::day04::read_input;
    use crate::utils;

    #[test]
    fn part1_example() {
        let game = read_input(utils::InputType::Example);
        assert_eq!(part1(&game), 4512);
    }

    #[test]
    fn part1_real() {
        let game = read_input(utils::InputType::Main);
        assert_eq!(part1(&game), 69579);
    }

    #[test]
    fn part2_example() {
        let game = read_input(utils::InputType::Example);
        assert_eq!(part2(&game), 1924);
    }

    #[test]
    fn part2_real() {
        let game = read_input(utils::InputType::Main);
        assert_eq!(part2(&game), 14877);
    }
}
