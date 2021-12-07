use crate::utils;

pub fn run(it: utils::InputType) {
    let input = read_input(it);
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}

fn part2(fish: &Vec<u8>) -> u64 {
    simulate_growth(256, fish)
        .iter()
        .sum::<u64>()
        .try_into()
        .unwrap()
}

fn part1(fish: &Vec<u8>) -> u64 {
    simulate_growth(80, fish)
        .iter()
        .sum::<u64>()
        .try_into()
        .unwrap()
}

fn simulate_growth(iterations: u32, fish: &Vec<u8>) -> Vec<u64> {
    let mut fish_pop = [0; 9];

    for f in fish {
        fish_pop[*f as usize] += 1
    }

    for _i in 0..iterations {
        let num_new = fish_pop[0];
        fish_pop.rotate_left(1);
        fish_pop[6] += num_new;
        fish_pop[8] = num_new;
    }
    fish_pop.to_vec()
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
