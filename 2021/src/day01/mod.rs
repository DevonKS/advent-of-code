use crate::utils;

pub fn run(it: utils::InputType) {
    let nums = read_input(it);
    println!("{}", part1(&nums));
    println!("{}", part2(&nums));
}

fn part2(nums: &Vec<u32>) -> u32 {
    nums.windows(4)
        .filter(|x| x[3] > x[0])
        .count()
        .try_into()
        .unwrap()
}

fn part1(nums: &Vec<u32>) -> u32 {
    nums.windows(2)
        .filter(|x| x[1] > x[0])
        .count()
        .try_into()
        .unwrap()
}

fn read_input(it: utils::InputType) -> Vec<u32> {
    utils::parse_file(utils::Day::Day01, it, |s| s.parse::<u32>().unwrap())
}

#[cfg(test)]
mod tests {
    use crate::day01::part1;
    use crate::day01::part2;
    use crate::day01::read_input;
    use crate::utils;

    #[test]
    fn part1_example() {
        let input = read_input(utils::InputType::Example);
        assert_eq!(part1(&input), 7);
    }

    #[test]
    fn part1_main() {
        let input = read_input(utils::InputType::Main);
        assert_eq!(part1(&input), 1581);
    }

    #[test]
    fn part2_example() {
        let input = read_input(utils::InputType::Example);
        assert_eq!(part2(&input), 5);
    }

    #[test]
    fn part2_main() {
        let input = read_input(utils::InputType::Main);
        assert_eq!(part2(&input), 1618);
    }
}
