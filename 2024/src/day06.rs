use std::collections::HashSet;

use crate::{time, util};

pub fn run(it: util::InputType) {
    let lab = read_input(it);
    time!("Part 1", println!("{}", part1(&lab)));
    time!("Part 2", println!("{}", part2(&lab)));
}

fn part1(lab: &Lab) -> usize {
    let mut new_lab = lab.clone();
    while new_lab.guard.is_some() {
        new_lab.move_guard();
    }

    new_lab.visited_squares.len()
}

fn part2(lab: &Lab) -> usize {
    lab.find_all_loop_obstacles().len()
}

#[derive(Clone)]
struct Lab {
    map: Vec<Vec<Tile>>,
    guard: Option<Guard>,
    visited_squares: HashSet<Position>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
struct Guard {
    position: Position,
    direction: Direction,
}

impl Lab {
    fn new(map: Vec<Vec<Tile>>) -> Self {
        let guard_pos_opt = Lab::find_guard_pos(&map);
        match guard_pos_opt {
            Some(guard_pos) => {
                let mut visited_squares = HashSet::new();
                visited_squares.insert(guard_pos);

                Lab {
                    map,
                    guard: Some(Guard {
                        position: guard_pos,
                        direction: Direction { row: -1, column: 0 },
                    }),
                    visited_squares,
                }
            }
            None => panic!("No guard in map"),
        }
    }

    fn find_guard_pos(map: &[Vec<Tile>]) -> Option<Position> {
        for (row_index, row) in map.iter().enumerate() {
            for (col_index, col) in row.iter().enumerate() {
                if *col == Tile::Guard {
                    return Some(Position {
                        row: row_index as isize,
                        column: col_index as isize,
                    });
                }
            }
        }

        None
    }

    fn move_guard(&mut self) {
        if self.guard.is_none() {
            return;
        }

        let guard = self.guard.unwrap();
        let next_guard_pos = Position {
            row: guard.position.row + guard.direction.row,
            column: guard.position.column + guard.direction.column,
        };

        if next_guard_pos.row < 0
            || next_guard_pos.column < 0
            || next_guard_pos.row >= self.map.len() as isize
            || next_guard_pos.column >= self.map[0].len() as isize
        {
            self.map[guard.position.row as usize][guard.position.column as usize] = Tile::Empty;
            self.guard = None;
        } else {
            self.map[guard.position.row as usize][guard.position.column as usize] = Tile::Empty;
            self.map[next_guard_pos.row as usize][next_guard_pos.column as usize] = Tile::Guard;
            self.guard = self.guard.map(|mut g| {
                g.position.row = next_guard_pos.row;
                g.position.column = next_guard_pos.column;
                g
            });
            self.visited_squares.insert(next_guard_pos);

            self.change_guard_direction_if_needed();
        }
    }

    fn change_guard_direction_if_needed(&mut self) {
        if self.guard.is_none() {
            return;
        }

        let mut guard = self.guard.unwrap();

        let mut next_guard_pos = Position {
            row: guard.position.row + guard.direction.row,
            column: guard.position.column + guard.direction.column,
        };

        while next_guard_pos.row >= 0
            && next_guard_pos.column >= 0
            && next_guard_pos.row < self.map.len() as isize
            && next_guard_pos.column < self.map[0].len() as isize
            && self.map[next_guard_pos.row as usize][next_guard_pos.column as usize]
                == Tile::Obstacle
        {
            if guard.direction.row == -1 && guard.direction.column == 0 {
                self.guard = self.guard.map(|mut g| {
                    g.direction.row = 0;
                    g.direction.column = 1;
                    g
                });
            } else if guard.direction.row == 0 && guard.direction.column == 1 {
                self.guard = self.guard.map(|mut g| {
                    g.direction.row = 1;
                    g.direction.column = 0;
                    g
                });
            } else if guard.direction.row == 1 && guard.direction.column == 0 {
                self.guard = self.guard.map(|mut g| {
                    g.direction.row = 0;
                    g.direction.column = -1;
                    g
                });
            } else if guard.direction.row == 0 && guard.direction.column == -1 {
                self.guard = self.guard.map(|mut g| {
                    g.direction.row = -1;
                    g.direction.column = 0;
                    g
                });
            }
            guard = self.guard.unwrap();

            next_guard_pos = Position {
                row: guard.position.row + guard.direction.row,
                column: guard.position.column + guard.direction.column,
            };
        }
    }

    fn find_all_loop_obstacles(&self) -> Vec<Position> {
        let mut loop_positions = Vec::new();

        if self.guard.is_none() {
            return loop_positions;
        }

        let mut new_lab = self.clone();
        while new_lab.guard.is_some() {
            new_lab.move_guard();
        }

        let guard_pos = self.guard.unwrap().position;

        for pos in new_lab.visited_squares {
            if pos != guard_pos && self.is_loop_obstacle(pos) {
                loop_positions.push(pos);
            }
        }

        loop_positions
    }

    fn is_loop_obstacle(&self, pos: Position) -> bool {
        let mut new_lab = self.clone();
        new_lab.map[pos.row as usize][pos.column as usize] = Tile::Obstacle;

        let mut seen_guards = HashSet::new();

        new_lab.change_guard_direction_if_needed();

        while new_lab.guard.is_some() {
            seen_guards.insert(new_lab.guard.unwrap());

            new_lab.move_guard();

            if new_lab.guard.is_some() && seen_guards.contains(&new_lab.guard.unwrap()) {
                return true;
            }
        }

        false
    }
}

#[derive(Clone, PartialEq)]
enum Tile {
    Obstacle,
    Guard,
    Empty,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
struct Position {
    row: isize,
    column: isize,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
struct Direction {
    row: isize,
    column: isize,
}

fn read_input(it: util::InputType) -> Lab {
    let map = util::parse_file(util::Day::Day06, it, |l| {
        l.chars()
            .map(|c| match c {
                '#' => Tile::Obstacle,
                '^' => Tile::Guard,
                '.' => Tile::Empty,
                _ => Tile::Empty,
            })
            .collect()
    });

    Lab::new(map)
}

#[cfg(test)]
mod tests {
    use crate::day06::part1;
    use crate::day06::part2;
    use crate::day06::read_input;
    use crate::util;

    #[test]
    fn part1_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part1(&input), 41);
    }

    #[test]
    fn part1_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part1(&input), 4711);
    }

    #[test]
    fn part2_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part2(&input), 6);
    }

    #[test]
    #[ignore]
    fn part2_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part2(&input), 1562);
    }
}
