use crate::utils;

use std::collections::HashMap;

pub fn run(it: utils::InputType) {
    let input = read_input(it);
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}

fn part2(fish: &Vec<u8>) -> u64 {
    let mut fish_map: HashMap<u8, u64> = HashMap::new();
    fish_map.insert(0, 0);
    fish_map.insert(1, 0);
    fish_map.insert(2, 0);
    fish_map.insert(3, 0);
    fish_map.insert(4, 0);
    fish_map.insert(5, 0);
    fish_map.insert(6, 0);
    fish_map.insert(7, 0);
    fish_map.insert(8, 0);

    for f in fish {
        *fish_map.get_mut(f).unwrap() += 1
    }

    for _i in 0..256 {
        let num_new = fish_map[&0];
        for i in 0..8 {
            *fish_map.get_mut(&i).unwrap() = fish_map[&(i + 1)];
        }

        *fish_map.get_mut(&6).unwrap() += num_new;
        *fish_map.get_mut(&8).unwrap() = num_new;
    }
    fish_map.values().sum()
}

fn part1(fish: &Vec<u8>) -> u32 {
    let mut local_fish = fish.clone();
    for _i in 0..80 {
        for i in 0..local_fish.len() {
            let f = local_fish[i];
            if f == 0 {
                local_fish[i] = 6;
                local_fish.push(8);
            } else {
                local_fish[i] -= 1;
            }
        }
    }
    local_fish.len().try_into().unwrap()
}

fn read_input(it: utils::InputType) -> Vec<u8> {
    utils::read_file(utils::Day::Day06, it)
        .trim()
        .split(',')
        .map(|s| s.parse().unwrap())
        .collect::<Vec<u8>>()
}

#[cfg(test)]
mod tests {
    use crate::day06::part1;
    use crate::day06::part2;
    use crate::day06::read_input;
    use crate::utils;

    #[test]
    fn part1_example() {
        let fish = read_input(utils::InputType::Example);
        assert_eq!(part1(&fish), 5934);
    }

    #[test]
    fn part1_real() {
        let lines = read_input(utils::InputType::Main);
        assert_eq!(part1(&lines), 373378);
    }

    #[test]
    fn part2_example() {
        let fish = read_input(utils::InputType::Example);
        assert_eq!(part2(&fish), 26984457539);
    }

    #[test]
    fn part2_real() {
        let lines = read_input(utils::InputType::Main);
        assert_eq!(part2(&lines), 1682576647495);
    }
}
