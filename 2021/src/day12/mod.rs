use crate::time;
use crate::utils;

use counter::Counter;

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
    let mut temp_paths: Vec<Vec<&str>> = Vec::new();
    let mut final_paths: Vec<Vec<&str>> = Vec::new();
    for child in &graph[start_node] {
        temp_paths.push(vec![start_node, &child]);
    }

    while temp_paths.len() > 0 {
        let path = temp_paths.pop().unwrap();
        let last_node: &str = *path.last().unwrap();
        for child in &graph[last_node] {
            if child == start_node {
                continue;
            }

            let mut new_path = path.clone();
            new_path.push(child);
            if child == end_node {
                final_paths.push(new_path);
            } else {
                let small_caves_counts = new_path
                    .iter()
                    .skip(1)
                    .filter(|&s| s.chars().all(|c| c.is_ascii_lowercase()))
                    .collect::<Counter<_>>();
                let num_small_cave_visits =
                    small_caves_counts.iter().map(|(_k, v)| *v).sum::<usize>();
                if num_small_cave_visits <= small_caves_counts.len() + 1 {
                    temp_paths.push(new_path);
                }
            }
        }
    }

    final_paths.len().try_into().unwrap()
}

fn part1(graph: &HashMap<String, HashSet<String>>) -> u64 {
    let start_node: &str = "start";
    let end_node: &str = "end";
    let mut temp_paths: Vec<Vec<&str>> = Vec::new();
    let mut final_paths: Vec<Vec<&str>> = Vec::new();
    for child in &graph[start_node] {
        temp_paths.push(vec![start_node, &child]);
    }

    while temp_paths.len() > 0 {
        let path = temp_paths.pop().unwrap();
        let last_node: &str = *path.last().unwrap();
        for child in &graph[last_node] {
            if child == start_node {
                continue;
            }

            let mut new_path = path.clone();
            new_path.push(child);
            if child == end_node {
                final_paths.push(new_path);
            } else {
                let small_caves_counts = new_path
                    .iter()
                    .skip(1)
                    .filter(|&s| s.chars().all(|c| c.is_ascii_lowercase()))
                    .collect::<Counter<_>>();
                if small_caves_counts.iter().all(|(_k, v)| *v <= 1) {
                    temp_paths.push(new_path);
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
