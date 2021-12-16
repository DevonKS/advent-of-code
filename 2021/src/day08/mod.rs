use crate::time;
use crate::utils;

use std::collections::HashMap;
use std::collections::HashSet;

pub fn run(it: utils::InputType) {
    let input = read_input(it);
    time!("Part 1", println!("{}", part1(&input)));
    time!("Part 2", println!("{}", part2(&input)));
}

fn part2(entries: &Vec<Entry>) -> u32 {
    let mut total: u32 = 0;
    for entry in entries {
        let mapping = determine_mapping(&entry);

        let mut output_val: String = String::new();
        for x in &entry.output_values {
            output_val.push(decode_value(&x, &mapping));
        }
        total += output_val.parse::<u32>().unwrap();
    }
    total
}

fn determine_mapping(entry: &Entry) -> HashMap<char, char> {
    let all_chars = HashSet::from(['a', 'b', 'c', 'd', 'e', 'f', 'g']);
    let mut possible_mappings: HashMap<char, HashSet<char>> = HashMap::from([
        ('a', all_chars.clone()),
        ('b', all_chars.clone()),
        ('c', all_chars.clone()),
        ('d', all_chars.clone()),
        ('e', all_chars.clone()),
        ('f', all_chars.clone()),
        ('g', all_chars.clone()),
    ]);

    let mut cs_6: HashSet<char> = HashSet::new();
    for pattern in &entry.signal_patterns {
        let l = pattern.len();
        if l == 2 {
            update_possible_mappings(
                &['c', 'f'].to_vec(),
                &['a', 'b', 'd', 'e', 'g'].to_vec(),
                pattern,
                &mut possible_mappings,
            );
        } else if l == 3 {
            update_possible_mappings(
                &['a', 'c', 'f'].to_vec(),
                &['b', 'd', 'e', 'g'].to_vec(),
                pattern,
                &mut possible_mappings,
            );
        } else if l == 4 {
            update_possible_mappings(
                &['b', 'c', 'd', 'f'].to_vec(),
                &['a', 'e', 'g'].to_vec(),
                pattern,
                &mut possible_mappings,
            );
        } else if l == 6 {
            cs_6.insert(*all_chars.difference(&pattern).next().unwrap());
        }
    }

    update_possible_mappings(
        &['c', 'd', 'e'].to_vec(),
        &['a', 'b', 'c', 'f', 'g'].to_vec(),
        &cs_6,
        &mut possible_mappings,
    );

    assert!(possible_mappings.iter().all(|(_k, v)| v.len() == 1));

    let mut mappings: HashMap<char, char> = HashMap::new();
    for (k, v) in possible_mappings {
        mappings.insert(*v.iter().next().unwrap(), k);
    }
    mappings
}

fn update_possible_mappings(
    in_set: &Vec<char>,
    out_set: &Vec<char>,
    chars: &HashSet<char>,
    possible_mappings: &mut HashMap<char, HashSet<char>>,
) {
    in_set.iter().for_each(|c| {
        if possible_mappings[c].len() > 1 {
            *possible_mappings.get_mut(c).unwrap() = possible_mappings[c]
                .intersection(&chars)
                .map(|c| *c)
                .collect()
        }
    });
    out_set.iter().for_each(|c| {
        if possible_mappings[c].len() > 1 {
            *possible_mappings.get_mut(c).unwrap() = possible_mappings[c]
                .difference(&chars)
                .map(|c| *c)
                .collect();
        }
    });
}

fn decode_value(val: &HashSet<char>, mapping: &HashMap<char, char>) -> char {
    let mut value: HashSet<char> = HashSet::new();
    val.iter().for_each(|c| {
        value.insert(mapping[&c]);
    });

    if HashSet::from(['a', 'b', 'c', 'e', 'f', 'g']) == value {
        return '0';
    } else if HashSet::from(['c', 'f']) == value {
        return '1';
    } else if HashSet::from(['a', 'c', 'd', 'e', 'g']) == value {
        return '2';
    } else if HashSet::from(['a', 'c', 'd', 'f', 'g']) == value {
        return '3';
    } else if HashSet::from(['b', 'c', 'd', 'f']) == value {
        return '4';
    } else if HashSet::from(['a', 'b', 'd', 'f', 'g']) == value {
        return '5';
    } else if HashSet::from(['a', 'b', 'd', 'e', 'f', 'g']) == value {
        return '6';
    } else if HashSet::from(['a', 'c', 'f']) == value {
        return '7';
    } else if HashSet::from(['a', 'b', 'c', 'd', 'e', 'f', 'g']) == value {
        return '8';
    } else if HashSet::from(['a', 'b', 'c', 'd', 'f', 'g']) == value {
        return '9';
    } else {
        panic!(
            "Unknown value.\nOriginal: {:?}\nAfter Mapping: {:?}",
            val, value
        );
    }
}

fn part1(entries: &Vec<Entry>) -> u32 {
    entries
        .iter()
        .flat_map(|e| e.output_values.clone())
        .filter(|v| {
            let l = v.len();
            l == 2 || l == 4 || l == 3 || l == 7
        })
        .count()
        .try_into()
        .unwrap()
}

fn read_input(it: utils::InputType) -> Vec<Entry> {
    utils::parse_file(utils::Day::Day08, it, |s| parse_entry(s))
}

fn parse_entry(s: &str) -> Entry {
    let x: Vec<&str> = s.split(" | ").collect();
    if x.len() != 2 {
        panic!("Bad Input");
    }
    let signals_string = x[0];
    let outputs_string = x[1];
    let signal_patterns = signals_string
        .split(' ')
        .map(|s| s.chars().collect())
        .collect();
    let output_values = outputs_string
        .split(' ')
        .map(|s| s.chars().collect())
        .collect();
    let entry = Entry {
        signal_patterns,
        output_values,
    };
    entry
}

struct Entry {
    signal_patterns: Vec<HashSet<char>>,
    output_values: Vec<HashSet<char>>,
}

#[cfg(test)]
mod tests {
    use crate::day08::part1;
    use crate::day08::part2;
    use crate::day08::read_input;
    use crate::utils;

    #[test]
    fn part1_example() {
        let entries = read_input(utils::InputType::Example);
        assert_eq!(part1(&entries), 26);
    }

    #[test]
    fn part1_real() {
        let entries = read_input(utils::InputType::Main);
        assert_eq!(part1(&entries), 239);
    }

    #[test]
    fn part2_example() {
        let entries = read_input(utils::InputType::Example);
        assert_eq!(part2(&entries), 61229);
    }

    #[test]
    fn part2_real() {
        let entries = read_input(utils::InputType::Main);
        assert_eq!(part2(&entries), 946346);
    }
}
