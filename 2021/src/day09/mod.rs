use crate::utils;

use std::collections::{HashSet, VecDeque};

pub fn run(it: utils::InputType) {
    let input = read_input(it);
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}

fn part2(nums: &Vec<Vec<u32>>) -> u32 {
    let max_row: u32 = (nums.len() - 1) as u32;
    let max_column: u32 = (nums[0].len() - 1) as u32;
    let low_points = find_low_points(nums);
    let mut basins: Vec<HashSet<(u32, u32)>> = Vec::new();

    for low_point in low_points {
        let mut basin: HashSet<(u32, u32)> = HashSet::new();
        basin.insert(low_point);

        let mut visited: HashSet<(u32, u32)> = HashSet::new();
        visited.insert(low_point);

        let mut queue: VecDeque<(u32, u32)> = VecDeque::new();
        for neighbour in get_neighbours(low_point, max_row, max_column) {
            queue.push_back(neighbour);
        }

        while queue.len() > 0 {
            let pos = queue.pop_front().unwrap();
            let (r, c) = pos;

            visited.insert(pos);

            if nums[r as usize][c as usize] < 9 {
                basin.insert(pos);
                for x in get_neighbours(pos, max_row, max_column) {
                    if !visited.contains(&x) && !queue.contains(&x) {
                        queue.push_back(x);
                    }
                }
            }
        }
        basins.push(basin);
    }

    basins.sort_by(|v1, v2| v1.len().partial_cmp(&v2.len()).unwrap().reverse());

    basins.iter().take(3).map(|b| b.len() as u32).product()
}

fn part1(nums: &Vec<Vec<u32>>) -> u32 {
    find_low_points(nums)
        .iter()
        .map(|&(r, c)| nums[r as usize][c as usize] + 1)
        .sum()
}

fn find_low_points(nums: &Vec<Vec<u32>>) -> Vec<(u32, u32)> {
    let max_row: u32 = (nums.len() - 1) as u32;
    let max_column: u32 = (nums[0].len() - 1) as u32;
    let mut low_points: Vec<(u32, u32)> = Vec::new();
    for (r, row) in nums.iter().enumerate() {
        for (c, n) in row.iter().enumerate() {
            let neighbours = get_neighbours((r as u32, c as u32), max_row, max_column);
            let mut low_point = true;
            for (neighbour_r, neighbour_c) in &neighbours {
                if *n >= nums[*neighbour_r as usize][*neighbour_c as usize] {
                    low_point = false;
                    break;
                }
            }

            if low_point {
                low_points.push((r as u32, c as u32))
            }
        }
    }
    low_points
}

fn get_neighbours(pos: (u32, u32), max_row: u32, max_column: u32) -> Vec<(u32, u32)> {
    let directions: [(i32, i32); 4] = [(0, 1), (0, -1), (1, 0), (-1, 0)];
    let r: i32 = pos.0 as i32;
    let c: i32 = pos.1 as i32;
    let mut neighbours: Vec<(u32, u32)> = Vec::new();
    for (rd, cd) in directions {
        let new_r = r + rd;
        let new_c = c + cd;
        if in_scope((new_r, new_c), max_row, max_column) {
            neighbours.push((new_r as u32, new_c as u32));
        }
    }
    neighbours
}

fn in_scope(pos: (i32, i32), max_row: u32, max_column: u32) -> bool {
    let (r, c) = pos;
    r >= 0 && r as u32 <= max_row && c >= 0 && c as u32 <= max_column
}

fn read_input(it: utils::InputType) -> Vec<Vec<u32>> {
    utils::parse_file(utils::Day::Day09, it, |s| {
        s.chars().map(|c| c.to_digit(10).unwrap()).collect()
    })
}

#[cfg(test)]
mod tests {
    use crate::day09::part1;
    use crate::day09::part2;
    use crate::day09::read_input;
    use crate::utils;

    #[test]
    fn part1_example() {
        let nums = read_input(utils::InputType::Example);
        assert_eq!(part1(&nums), 15);
    }

    #[test]
    fn part1_real() {
        let nums = read_input(utils::InputType::Main);
        assert_eq!(part1(&nums), 633);
    }

    #[test]
    fn part2_example() {
        let nums = read_input(utils::InputType::Example);
        assert_eq!(part2(&nums), 1134);
    }

    #[test]
    fn part2_real() {
        let nums = read_input(utils::InputType::Main);
        assert_eq!(part2(&nums), 1050192);
    }
}
