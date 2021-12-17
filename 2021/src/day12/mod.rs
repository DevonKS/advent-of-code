use crate::time;
use crate::utils;

use std::collections::HashMap;
use std::collections::HashSet;

pub fn run(it: utils::InputType) {
    let input = read_input(it);
    time!("Part 1", println!("{}", part1(&input)));
    time!("Part 2", println!("{}", part2(&input)));
}

fn part2(graph: &HashMap<String, HashSet<String>>) -> u64 {
    let start_node: &str = "start";
    let end_node: &str = "end";
    let mut final_paths: Vec<Vec<&str>> = Vec::new();

    let mut stack: Vec<(Vec<&str>, HashSet<&str>, bool)> = Vec::new();
    stack.push((vec![start_node], HashSet::new(), false));
    while stack.len() > 0 {
        let (path, seen, twice) = stack.pop().unwrap();
        let last_node: &str = *path.last().unwrap();
        for child in &graph[last_node] {
            if child == start_node {
                continue;
            }

            if child == end_node {
                let mut new_path = path.clone();
                new_path.push(child);
                final_paths.push(new_path);
                continue;
            }

            let is_small_cave = child.chars().all(|c| c.is_ascii_lowercase());

            if is_small_cave {
                let child_seen = seen.contains(child as &str);
                if !child_seen || !twice {
                    let mut new_path = path.clone();
                    new_path.push(child);
                    let mut new_seen = seen.clone();
                    new_seen.insert(&child);
                    stack.push((new_path, new_seen, child_seen || twice))
                }
            } else {
                let mut new_path = path.clone();
                new_path.push(child);
                stack.push((new_path, seen.clone(), twice))
            }
        }
    }

    final_paths.len().try_into().unwrap()
}

fn part1(graph: &HashMap<String, HashSet<String>>) -> u64 {
    let start_node: &str = "start";
    let end_node: &str = "end";
    let mut final_paths: Vec<Vec<&str>> = Vec::new();

    let mut stack: Vec<(Vec<&str>, HashSet<&str>)> = Vec::new();
    stack.push((vec![start_node], HashSet::new()));
    while stack.len() > 0 {
        let (path, seen) = stack.pop().unwrap();
        let last_node: &str = *path.last().unwrap();
        for child in &graph[last_node] {
            if child == start_node {
                continue;
            }

            if child == end_node {
                let mut new_path = path.clone();
                new_path.push(child);
                final_paths.push(new_path);
            } else {
                let is_small_cave = child.chars().all(|c| c.is_ascii_lowercase());

                if is_small_cave {
                    if !seen.contains(child as &str) {
                        let mut new_path = path.clone();
                        new_path.push(child);
                        let mut new_seen = seen.clone();
                        new_seen.insert(&child);
                        stack.push((new_path, new_seen))
                    }
                } else {
                    let mut new_path = path.clone();
                    new_path.push(child);
                    stack.push((new_path, seen.clone()))
                }
            }
        }
    }

    final_paths.len().try_into().unwrap()
}

fn read_input(it: utils::InputType) -> HashMap<String, HashSet<String>> {
    read_string(&utils::read_file(utils::Day::Day12, it))
}

fn read_string(s: &str) -> HashMap<String, HashSet<String>> {
    let mut graph: HashMap<String, HashSet<String>> = HashMap::new();
    s.lines().for_each(|s| {
        let parts: Vec<&str> = s.split('-').collect();
        assert_eq!(2, parts.len());
        let a = parts[0];
        let b = parts[1];

        if !graph.contains_key(a) {
            graph.insert(a.to_string(), HashSet::new());
        }
        if !graph.contains_key(b) {
            graph.insert(b.to_string(), HashSet::new());
        }

        graph.get_mut(a).unwrap().insert(b.to_string());
        graph.get_mut(b).unwrap().insert(a.to_string());
    });
    graph
}

#[cfg(test)]
mod tests {
    use crate::day12::part1;
    use crate::day12::part2;
    use crate::day12::read_input;
    use crate::day12::read_string;
    use crate::utils;

    #[test]
    fn part1_example() {
        let graph = read_input(utils::InputType::Example);
        assert_eq!(part1(&graph), 10);
    }

    #[test]
    fn part1_example2() {
        let s = "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc";
        let graph = read_string(s);
        assert_eq!(part1(&graph), 19);
    }

    #[test]
    fn part1_example3() {
        let s = "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW";
        let graph = read_string(s);
        assert_eq!(part1(&graph), 226);
    }

    #[test]
    fn part1_real() {
        let graph = read_input(utils::InputType::Main);
        assert_eq!(part1(&graph), 3421);
    }

    #[test]
    fn part2_example() {
        let graph = read_input(utils::InputType::Example);
        assert_eq!(part2(&graph), 36);
    }

    #[test]
    fn part2_example2() {
        let s = "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc";
        let graph = read_string(s);
        assert_eq!(part2(&graph), 103);
    }

    #[test]
    fn part2_example3() {
        let s = "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW";
        let graph = read_string(s);
        assert_eq!(part2(&graph), 3509);
    }

    #[test]
    #[ignore]
    fn part2_real() {
        let graph = read_input(utils::InputType::Main);
        assert_eq!(part2(&graph), 84870);
    }
}
