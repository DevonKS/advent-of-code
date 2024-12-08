use std::collections::HashMap;
use std::iter::zip;

use regex::Regex;

use crate::time;
use crate::util;

pub fn run(it: util::InputType) {
    let nums = read_input(it);
    time!("Part 1", println!("{}", part1(&nums)));
    time!("Part 2", println!("{}", part2(&nums)));
}

fn part1(lists: &(Vec<isize>, Vec<isize>)) -> usize {
    let mut list1 = lists.0.clone();
    list1.sort();
    let mut list2 = lists.1.clone();
    list2.sort();

    let mut total: usize = 0;
    for (a, b) in zip(list1, list2) {
        total += usize::try_from((a - b).abs()).unwrap();
    }

    total
}

fn part2(lists: &(Vec<isize>, Vec<isize>)) -> usize {
    let mut right_list_counts: HashMap<isize, usize> = HashMap::new();
    for i in lists.1.iter() {
        right_list_counts
            .entry(*i)
            .and_modify(|i| *i += 1)
            .or_insert(1);
    }

    let mut total: usize = 0;
    for i in lists.0.iter() {
        if let Some(x) = right_list_counts.get(i) {
            total += *i as usize * x
        }
    }

    total
}

fn read_input(it: util::InputType) -> (Vec<isize>, Vec<isize>) {
    let mut list1 = Vec::new();
    let mut list2 = Vec::new();

    let input_str = util::read_file(util::Day::Day01, it);
    for l in input_str.lines() {
        let re = Regex::new(r"^(\d+)\s+(\d+)$").unwrap();
        let caps = re.captures(l).unwrap();
        list1.push(caps.get(1).unwrap().as_str().parse().unwrap());
        list2.push(caps.get(2).unwrap().as_str().parse().unwrap());
    }
    (list1, list2)
}

#[cfg(test)]
mod tests {
    use crate::day01::part1;
    use crate::day01::part2;
    use crate::day01::read_input;
    use crate::util;

    #[test]
    fn part1_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part1(&input), 11);
    }

    #[test]
    fn part1_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part1(&input), 1579939);
    }

    #[test]
    fn part2_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part2(&input), 31);
    }

    #[test]
    fn part2_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part2(&input), 20351745);
    }
}
