use priority_queue::PriorityQueue;

use crate::time;
use crate::utils;

use std::cmp::Reverse;
use std::collections::HashMap;
use std::collections::HashSet;

pub fn run(it: utils::InputType) {
    let input = read_input(it);
    time!("Part 1", println!("{}", part1(&input)));
    time!("Part 2", println!("{}", part2(&input)));
}

fn part2(cave: &Vec<Vec<u8>>) -> u64 {
    let entire_cave = build_entire_cave(cave);

    let max_r = u32::try_from(entire_cave.len()).unwrap();
    let max_c = u32::try_from(entire_cave[0].len()).unwrap();
    find_shortest_distance((0, 0), (max_r - 1, max_c - 1), &entire_cave)
}

fn part1(cave: &Vec<Vec<u8>>) -> u64 {
    let max_r = u32::try_from(cave.len()).unwrap();
    let max_c = u32::try_from(cave[0].len()).unwrap();

    find_shortest_distance((0, 0), (max_r - 1, max_c - 1), cave)
}

fn build_entire_cave(cave: &Vec<Vec<u8>>) -> Vec<Vec<u8>> {
    let max_r = u32::try_from(cave.len()).unwrap();
    let max_c = u32::try_from(cave[0].len()).unwrap();
    let num_tiles = 5;
    let mut new_cave: Vec<Vec<u8>> =
        std::iter::repeat(vec![0; (max_c * num_tiles).try_into().unwrap()])
            .take((max_r * num_tiles).try_into().unwrap())
            .collect();
    for (r, row) in cave.iter().enumerate() {
        for (c, item) in row.iter().enumerate() {
            for new_r in 0..5 {
                for new_c in 0..5 {
                    let mut new_val = item + new_r + new_c;
                    while new_val > 9 {
                        new_val -= 9;
                    }
                    new_cave[r + (new_r as usize * usize::try_from(max_r).unwrap())]
                        [c + (new_c as usize * usize::try_from(max_c).unwrap())] = new_val;
                }
            }
        }
    }
    new_cave
}

fn find_shortest_distance(start: (u32, u32), end: (u32, u32), cave: &Vec<Vec<u8>>) -> u64 {
    let max_r = u32::try_from(cave.len()).unwrap();
    let max_c = u32::try_from(cave[0].len()).unwrap();
    let directions: [(i32, i32); 4] = [(-1, 0), (0, 1), (1, 0), (0, -1)];

    let mut distances: HashMap<(u32, u32), u64> = HashMap::new();
    let mut pq = PriorityQueue::new();
    let mut unvisited: HashSet<(u32, u32)> = HashSet::new();
    for r in 0..max_r {
        for c in 0..max_c {
            let node = (r, c);
            let dist = if (r, c) == start { 0 } else { u64::MAX };
            pq.push(node, Reverse(dist));
            unvisited.insert(node);
            distances.insert(node, dist);
        }
    }

    while !unvisited.is_empty() {
        let current: (u32, u32) = pq.pop().unwrap().0;
        unvisited.remove(&current);

        let current_dist: u64 = distances[&current];

        directions
            .iter()
            .map(|(rd, cd)| {
                (
                    i32::try_from(current.0).unwrap() + rd,
                    i32::try_from(current.1).unwrap() + cd,
                )
            })
            .filter(|&(r, c)| {
                r >= 0
                    && r < max_r.try_into().unwrap()
                    && c >= 0
                    && c < max_c.try_into().unwrap()
                    && unvisited.contains(&(r.try_into().unwrap(), c.try_into().unwrap()))
            })
            .map(|(r, c)| (r.try_into().unwrap(), c.try_into().unwrap()))
            .for_each(|neighbour| {
                let new_cost = current_dist
                    + u64::try_from(
                        cave[usize::try_from(neighbour.0).unwrap()]
                            [usize::try_from(neighbour.1).unwrap()],
                    )
                    .unwrap();

                distances.entry(neighbour).and_modify(|e| {
                    if new_cost < *e {
                        *e = new_cost;
                        pq.change_priority(&neighbour, Reverse(new_cost));
                    }
                });
            });

        if current == end || (pq.peek().unwrap().1).0 == u64::MAX {
            break;
        }
    }
    distances[&end]
}

fn read_input(it: utils::InputType) -> Vec<Vec<u8>> {
    utils::parse_file(utils::Day::Day15, it, |s| {
        s.chars()
            .map(|c| c.to_digit(10).unwrap().try_into().unwrap())
            .collect()
    })
}

#[cfg(test)]
mod tests {
    use crate::day15::part1;
    use crate::day15::part2;
    use crate::day15::read_input;
    use crate::utils;

    #[test]
    fn part1_example() {
        let cave = read_input(utils::InputType::Example);
        assert_eq!(part1(&cave), 40);
    }

    #[test]
    fn part1_real() {
        let cave = read_input(utils::InputType::Main);
        assert_eq!(part1(&cave), 456);
    }

    #[test]
    fn part2_example() {
        let cave = read_input(utils::InputType::Example);
        assert_eq!(part2(&cave), 315);
    }

    #[test]
    #[ignore]
    fn part2_real() {
        let cave = read_input(utils::InputType::Main);
        assert_eq!(part2(&cave), 2831);
    }
}
