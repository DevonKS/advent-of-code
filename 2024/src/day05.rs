use std::collections::{HashMap, HashSet};

use regex::Regex;

use crate::{time, util};

pub fn run(it: util::InputType) {
    let input = read_input(it);
    time!("Part 1", println!("{}", part1(&input)));
    time!("Part 2", println!("{}", part2(&input)));
}

fn part1(input: &Input) -> usize {
    input
        .updates
        .iter()
        .filter(|u| valid_update(&input.before_rules, &input.after_rules, u))
        .map(|u| u[u.len() / 2])
        .sum()
}

fn part2(input: &Input) -> usize {
    input
        .updates
        .iter()
        .filter(|u| !valid_update(&input.before_rules, &input.after_rules, u))
        .map(|u| fix_update(&input.before_rules, &input.after_rules, u))
        .map(|u| u[u.len() / 2])
        .sum()
}

fn valid_update(
    before_rules: &HashMap<usize, HashSet<usize>>,
    after_rules: &HashMap<usize, HashSet<usize>>,
    update: &[usize],
) -> bool {
    for (i, u) in update.iter().enumerate() {
        let before_rule_set = before_rules.get(u);
        let after_rule_set = after_rules.get(u);

        if let Some(before_rule_set) = before_rule_set {
            for b in &update[0..i] {
                if !before_rule_set.contains(b) {
                    return false;
                }
            }
        }

        if let Some(after_rule_set) = after_rule_set {
            for a in &update[i + 1..update.len()] {
                if !after_rule_set.contains(a) {
                    return false;
                }
            }
        }
    }

    true
}

fn fix_update(
    before_rules: &HashMap<usize, HashSet<usize>>,
    after_rules: &HashMap<usize, HashSet<usize>>,
    update: &[usize],
) -> Vec<usize> {
    let mut fixed = update.to_vec();
    fixed.sort_by(|a, b| {
        let a_before_rule_set = before_rules.get(a);
        let a_after_rule_set = after_rules.get(a);
        let b_before_rule_set = before_rules.get(b);
        let b_after_rule_set = after_rules.get(b);

        if b_before_rule_set.is_some() && b_before_rule_set.unwrap().contains(a)
            || a_after_rule_set.is_some() && a_after_rule_set.unwrap().contains(b)
        {
            std::cmp::Ordering::Less
        } else if b_after_rule_set.is_some() && b_after_rule_set.unwrap().contains(a)
            || a_before_rule_set.is_some() && a_before_rule_set.unwrap().contains(b)
        {
            std::cmp::Ordering::Greater
        } else {
            std::cmp::Ordering::Equal
        }
    });

    fixed
}

struct Input {
    before_rules: HashMap<usize, HashSet<usize>>,
    after_rules: HashMap<usize, HashSet<usize>>,
    updates: Vec<Vec<usize>>,
}

fn read_input(it: util::InputType) -> Input {
    let input_str = util::read_file(util::Day::Day05, it);
    let parts: Vec<&str> = input_str.split("\n\n").collect();
    assert_eq!(2, parts.len());

    let re = Regex::new(r"^(\d+)\|(\d+)$").unwrap();
    let mut before_rules: HashMap<usize, HashSet<usize>> = HashMap::new();
    let mut after_rules: HashMap<usize, HashSet<usize>> = HashMap::new();
    for l in parts[0].lines() {
        let caps = re.captures(l).unwrap();
        let k = caps.get(1).unwrap().as_str().parse().unwrap();
        let v = caps.get(2).unwrap().as_str().parse().unwrap();
        after_rules.entry(k).or_default().insert(v);
        before_rules.entry(v).or_default().insert(k);
    }

    let updates = parts[1]
        .lines()
        .map(|l| l.split(",").map(|n| n.parse().unwrap()).collect())
        .collect();

    Input {
        before_rules,
        after_rules,
        updates,
    }
}

#[cfg(test)]
mod tests {
    use crate::day05::part1;
    use crate::day05::part2;
    use crate::day05::read_input;
    use crate::util;

    #[test]
    fn part1_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part1(&input), 143);
    }

    #[test]
    fn part1_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part1(&input), 4872);
    }

    #[test]
    fn part2_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part2(&input), 123);
    }

    #[test]
    fn part2_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part2(&input), 5564);
    }
}
