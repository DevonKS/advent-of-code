use crate::{time, util};

pub fn run(it: util::InputType) {
    let reports = read_input(it);
    time!("Part 1", println!("{}", part1(&reports)));
    time!("Part 2", println!("{}", part2(&reports)));
}

fn part1(reports: &[Vec<usize>]) -> usize {
    reports.iter().filter(|r| is_safe(r)).count()
}

fn part2(reports: &[Vec<usize>]) -> usize {
    let mut safe_reports = 0;
    for report in reports {
        if is_safe(report) {
            safe_reports += 1;
            continue;
        }

        for i in 0..report.len() {
            let mut new_report = report.clone();
            new_report.remove(i);

            if is_safe(&new_report) {
                safe_reports += 1;
                break;
            }
        }
    }

    safe_reports
}

fn is_safe(report: &[usize]) -> bool {
    let is_ascending = report[0] < report[1];

    for pair in report.windows(2) {
        let a = pair[0];
        let b = pair[1];

        if a == b {
            return false;
        }

        if is_ascending {
            if a > b {
                return false;
            }
        } else if a < b {
            return false;
        }

        let diff = a.abs_diff(b);
        if !(1..=3).contains(&diff) {
            return false;
        }
    }

    true
}

fn read_input(it: util::InputType) -> Vec<Vec<usize>> {
    util::parse_file(util::Day::Day02, it, |line| {
        line.split(" ")
            .map(|n| n.parse::<usize>().unwrap())
            .collect()
    })
}

#[cfg(test)]
mod tests {
    use crate::day02::part1;
    use crate::day02::part2;
    use crate::day02::read_input;
    use crate::util;

    #[test]
    fn part1_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part1(&input), 2);
    }

    #[test]
    fn part1_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part1(&input), 269);
    }

    #[test]
    fn part2_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part2(&input), 4);
    }

    #[test]
    fn part2_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part2(&input), 337);
    }
}
