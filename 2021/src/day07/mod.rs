use crate::utils;

pub fn run(it: utils::InputType) {
    let input = read_input(it);
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}

fn part2(nums: &Vec<u32>) -> u32 {
    let max = *nums.iter().max().unwrap();
    let mut min_fuel = std::u32::MAX;
    for i in 0..max + 1 {
        let mut fuel = 0;
        for n in nums {
            let x = ((*n as i32 - i as i32).abs()) as u32;
            fuel += (x * (x + 1)) / 2;
        }

        if fuel < min_fuel {
            min_fuel = fuel
        }
    }
    min_fuel
}

fn part1(nums: &Vec<u32>) -> u32 {
    let max = *nums.iter().max().unwrap();
    let mut min_fuel = std::u32::MAX;
    for i in 0..max + 1 {
        let mut fuel = 0;
        for n in nums {
            fuel += ((*n as i32 - i as i32).abs()) as u32;
        }

        if fuel < min_fuel {
            min_fuel = fuel
        }
    }
    min_fuel
}

fn read_input(it: utils::InputType) -> Vec<u32> {
    utils::read_file(utils::Day::Day07, it)
        .trim()
        .split(',')
        .map(|s| s.parse().unwrap())
        .collect::<Vec<u32>>()
}

#[cfg(test)]
mod tests {
    use crate::day07::part1;
    use crate::day07::part2;
    use crate::day07::read_input;
    use crate::utils;

    #[test]
    fn part1_example() {
        let nums = read_input(utils::InputType::Example);
        assert_eq!(part1(&nums), 37);
    }

    #[test]
    fn part1_real() {
        let nums = read_input(utils::InputType::Main);
        assert_eq!(part1(&nums), 344605);
    }

    #[test]
    fn part2_example() {
        let nums = read_input(utils::InputType::Example);
        assert_eq!(part2(&nums), 168);
    }

    #[test]
    fn part2_real() {
        let nums = read_input(utils::InputType::Main);
        assert_eq!(part2(&nums), 93699985);
    }
}
