use crate::{time, util};

pub fn run(it: util::InputType) {
    let wordsearch = read_input(it);
    time!("Part 1", println!("{}", part1(&wordsearch)));
    time!("Part 2", println!("{}", part2(&wordsearch)));
}

fn part1(wordsearch: &[String]) -> usize {
    find_matches(wordsearch, "XMAS")
}

fn part2(wordsearch: &[String]) -> usize {
    find_matches_2(wordsearch)
}

fn find_matches(wordsearch: &[String], word: &str) -> usize {
    let mut matches = 0;

    for l in wordsearch {
        matches += find_matches_str(l, word);
    }

    let all_chars: Vec<Vec<char>> = wordsearch.iter().map(|s| s.chars().collect()).collect();

    for i in 0..all_chars[0].len() {
        let s = all_chars.iter().map(|s| s[i]).collect::<String>();
        matches += find_matches_str(&s, word);
    }

    for i in 0..=all_chars[0].len() - word.len() {
        let s = all_chars
            .iter()
            .enumerate()
            .take_while(|(j, _)| i + j < all_chars[0].len())
            .map(|(j, s)| s[i + j])
            .collect::<String>();
        matches += find_matches_str(&s, word);
    }

    for i in 0..all_chars[0].len() - word.len() {
        let s = all_chars
            .iter()
            .enumerate()
            .skip(i + 1)
            .map(|(j, s)| s[j - (i + 1)])
            .collect::<String>();
        matches += find_matches_str(&s, word);
    }

    for i in 0..=all_chars[0].len() - word.len() {
        let s = all_chars
            .iter()
            .rev()
            .enumerate()
            .take_while(|(j, _)| i + j < all_chars[0].len())
            .map(|(j, s)| s[i + j])
            .collect::<String>();
        matches += find_matches_str(&s, word);
    }

    for i in 0..all_chars[0].len() - word.len() {
        let s = all_chars
            .iter()
            .rev()
            .enumerate()
            .skip(i + 1)
            .map(|(j, s)| s[j - (i + 1)])
            .collect::<String>();
        matches += find_matches_str(&s, word);
    }

    matches
}

fn find_matches_str(s: &str, word: &str) -> usize {
    s.matches(word).count() + s.chars().rev().collect::<String>().matches(word).count()
}

fn find_matches_2(wordsearch: &[String]) -> usize {
    let all_chars: Vec<Vec<char>> = wordsearch.iter().map(|s| s.chars().collect()).collect();
    let mut total = 0;

    for i in 0..=all_chars.len() - 3 {
        for j in 0..=all_chars[i].len() - 3 {
            let mut block = Vec::with_capacity(3);
            for k in 0..3 {
                let n = all_chars[i + k][j..j + 3].to_vec();
                block.push(n);
            }
            if check_block(&block) {
                total += 1;
            }
        }
    }

    total
}

fn check_block(block: &[Vec<char>]) -> bool {
    if block[0][0] == 'M'
        && block[0][2] == 'S'
        && block[2][0] == 'M'
        && block[2][2] == 'S'
        && block[1][1] == 'A'
    {
        return true;
    }

    if block[0][0] == 'S'
        && block[0][2] == 'M'
        && block[2][0] == 'S'
        && block[2][2] == 'M'
        && block[1][1] == 'A'
    {
        return true;
    }

    if block[0][0] == 'M'
        && block[0][2] == 'M'
        && block[2][0] == 'S'
        && block[2][2] == 'S'
        && block[1][1] == 'A'
    {
        return true;
    }

    if block[0][0] == 'S'
        && block[0][2] == 'S'
        && block[2][0] == 'M'
        && block[2][2] == 'M'
        && block[1][1] == 'A'
    {
        return true;
    }

    false
}

fn read_input(it: util::InputType) -> Vec<String> {
    util::parse_file(util::Day::Day04, it, |s| s.to_string())
}

#[cfg(test)]
mod tests {
    use crate::day04::part1;
    use crate::day04::part2;
    use crate::day04::read_input;
    use crate::util;

    #[test]
    fn part1_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part1(&input), 18);
    }

    #[test]
    fn part1_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part1(&input), 2514);
    }

    #[test]
    fn part2_example() {
        let input = read_input(util::InputType::Example);
        assert_eq!(part2(&input), 9);
    }

    #[test]
    fn part2_main() {
        let input = read_input(util::InputType::Main);
        assert_eq!(part2(&input), 1888);
    }
}
