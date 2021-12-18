use crate::time;
use crate::utils;

use counter::Counter;
use regex::Regex;
use std::collections::HashMap;

pub fn run(it: utils::InputType) {
    let input = read_input(it);
    time!("Part 1", println!("{}", part1(&input)));
    time!("Part 2", println!("{}", part2(&input)));
}

fn part2(input: &Input) -> u64 {
    let mut pair_counts: HashMap<String, u64> = HashMap::new();
    for i in 0..input.template.chars().count() - 1 {
        let pair = &input.template[i..i + 2];
        pair_counts
            .entry(pair.to_string())
            .and_modify(|e| *e += 1)
            .or_insert(1);
    }

    let mut char_counts: HashMap<char, u64> = HashMap::new();
    input.template.chars().for_each(|c| {
        char_counts.entry(c).and_modify(|e| *e += 1).or_insert(1);
    });

    for _i in 0..40 {
        let mut changes: HashMap<String, i64> = HashMap::new();
        for rule in &input.rules {
            if pair_counts.contains_key(&rule.pair as &str) && pair_counts[&rule.pair as &str] != 0
            {
                let count: i64 = pair_counts[&rule.pair as &str].try_into().unwrap();
                let pair = &rule.pair;
                changes
                    .entry(pair.to_string())
                    .and_modify(|e| *e -= count)
                    .or_insert(-count);

                let new_pair1 = format!("{}{}", pair.chars().nth(0).unwrap(), rule.insertion);
                changes
                    .entry(new_pair1)
                    .and_modify(|e| *e += count)
                    .or_insert(count);

                let new_pair2 = format!("{}{}", rule.insertion, pair.chars().nth(1).unwrap());
                changes
                    .entry(new_pair2)
                    .and_modify(|e| *e += count)
                    .or_insert(count);

                char_counts
                    .entry(rule.insertion)
                    .and_modify(|e| {
                        *e += u64::try_from(count).unwrap();
                    })
                    .or_insert(u64::try_from(count).unwrap());
            }
        }

        for (k, v) in changes {
            if v != 0 {
                pair_counts
                    .entry(k)
                    .and_modify(|e| {
                        *e = u64::try_from(i64::try_from(*e).unwrap() + v).unwrap();
                    })
                    .or_insert(u64::try_from(v).unwrap_or(0));
            }
        }
    }

    let mut max_count: u64 = 0;
    let mut min_count = u64::MAX;
    char_counts.iter().for_each(|(_k, v)| {
        if *v > max_count {
            max_count = *v;
        }

        if *v < min_count {
            min_count = *v;
        }
    });

    max_count - min_count
}

fn part1(input: &Input) -> u64 {
    let mut temp: String = input.template.clone();
    for _i in 0..10 {
        temp = run_step(&temp, &input.rules);
    }
    let counts = temp.chars().collect::<Counter<_>>();

    let mut max_count: u64 = 0;
    let mut min_count = u64::MAX;
    counts.iter().for_each(|(_k, v)| {
        let val = u64::try_from(*v).unwrap();
        if val > max_count {
            max_count = val;
        }

        if val < min_count {
            min_count = val;
        }
    });

    max_count - min_count
}

fn run_step(template: &str, rules: &Vec<Rule>) -> String {
    let mut changes: Vec<(char, usize)> = Vec::new();
    for rule in rules {
        find_indices(template, &rule.pair)
            .iter()
            .for_each(|i| changes.push((rule.insertion, i + 1)))
    }

    changes.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
    let mut new_template = template.to_string();
    for (i, change) in changes.iter().enumerate() {
        new_template.insert(change.1 + i, change.0);
    }
    new_template
}

fn find_indices(s: &str, sub: &str) -> Vec<usize> {
    let mut indices: Vec<usize> = Vec::new();
    let len_sub = sub.chars().count();

    for i in 0..(s.chars().count() - len_sub + 1) {
        if s[i..(i + len_sub)] == *sub {
            indices.push(i);
        }
    }
    indices
}

fn read_input(it: utils::InputType) -> Input {
    let input_str = utils::read_file(utils::Day::Day14, it);
    let parts: Vec<&str> = input_str.split("\n\n").collect();
    assert_eq!(2, parts.len());

    let rules: Vec<Rule> = parts[1]
        .lines()
        .map(|s| {
            let re = Regex::new(r"^([A-Z]+) -> ([A-Z]+)$").unwrap();
            let caps = re.captures(s).unwrap();
            Rule {
                pair: caps.get(1).unwrap().as_str().to_string(),
                insertion: caps.get(2).unwrap().as_str().chars().next().unwrap(),
            }
        })
        .collect();

    Input {
        template: parts[0].to_string(),
        rules,
    }
}

struct Input {
    template: String,
    rules: Vec<Rule>,
}

#[derive(Debug)]
struct Rule {
    pair: String,
    insertion: char,
}

#[cfg(test)]
mod tests {
    use crate::day14::part1;
    use crate::day14::part2;
    use crate::day14::read_input;
    use crate::utils;

    #[test]
    fn part1_example() {
        let input = read_input(utils::InputType::Example);
        assert_eq!(part1(&input), 1588);
    }

    #[test]
    fn part1_real() {
        let input = read_input(utils::InputType::Main);
        assert_eq!(part1(&input), 4244);
    }

    #[test]
    fn part2_example() {
        let input = read_input(utils::InputType::Example);
        assert_eq!(part2(&input), 2188189693529);
    }

    #[test]
    fn part2_real() {
        let input = read_input(utils::InputType::Main);
        assert_eq!(part2(&input), 4807056953866);
    }
}
