use crate::utils;

pub fn run(it: utils::InputType) {
    let input = read_input(it);
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}

fn part2(lines: &Vec<Vec<Chunk>>) -> u64 {
    let mut scores: Vec<u64> = lines
        .iter()
        .filter(|&vs| !is_corrupted(vs))
        .map(|vs| {
            let completion_string = completion_string(vs);
            completion_string_score(&completion_string)
        })
        .collect();
    scores.sort();
    let middle_index = scores.len() / 2;
    scores[middle_index]
}

fn part1(lines: &Vec<Vec<Chunk>>) -> u32 {
    lines
        .iter()
        .map(|vs| {
            let first_corrupted = first_corrupted(vs);
            if first_corrupted.is_none() {
                0
            } else {
                match first_corrupted.unwrap().end_kind.unwrap() {
                    BracketKind::Round => 3,
                    BracketKind::Square => 57,
                    BracketKind::Curly => 1197,
                    BracketKind::Angled => 25137,
                }
            }
        })
        .sum()
}

fn read_input(it: utils::InputType) -> Vec<Vec<Chunk>> {
    utils::parse_file(utils::Day::Day10, it, |s| parse(&s.chars().collect()))
}

fn completion_string(vs: &Vec<Chunk>) -> String {
    let mut s = String::new();
    for chunk in vs {
        s.push_str(&completion_string(&chunk.sub_chunks));
        if chunk.state == ParseState::Incomplete {
            s.push(bracket_kind_to_closing_char(&chunk.start_kind))
        }
    }
    s
}

fn completion_string_score(s: &str) -> u64 {
    let mut score: u64 = 0;
    for c in s.chars() {
        let char_value = match c {
            ')' => 1,
            ']' => 2,
            '}' => 3,
            '>' => 4,
            _ => 0,
        };
        score = score * 5 + char_value;
    }
    score
}

fn first_corrupted(vs: &Vec<Chunk>) -> Option<Chunk> {
    let mut first: Option<Chunk> = None;
    for chunk in vs {
        if chunk.state == ParseState::Corrupted {
            first = Some(chunk.clone());
            break;
        }
        let first_child = first_corrupted(&chunk.sub_chunks);
        if first_child.is_some() {
            first = first_child;
            break;
        }
    }
    first
}

fn is_corrupted(vs: &Vec<Chunk>) -> bool {
    let mut corrupted = false;
    for chunk in vs {
        if chunk.state == ParseState::Corrupted {
            corrupted = true;
            break;
        }
        let child_corrupted = is_corrupted(&chunk.sub_chunks);
        if child_corrupted {
            corrupted = true;
            break;
        }
    }
    corrupted
}

fn parse(s: &Vec<char>) -> Vec<Chunk> {
    let mut chunks = Vec::new();
    let end = s.len();
    let mut current_index = 0;
    while current_index < end {
        let (chunk, new_index) = parse_one(s, Some(current_index));
        if chunk.is_some() {
            chunks.push(chunk.unwrap());
        }
        current_index = new_index;
    }
    chunks
}

fn parse_one(s: &Vec<char>, start_index: Option<usize>) -> (Option<Chunk>, usize) {
    let start_index = start_index.unwrap_or(0);
    let current_char = s[start_index];
    if !(current_char == '(' || current_char == '[' || current_char == '<' || current_char == '{') {
        return (None, start_index);
    }

    let end = s.len();

    let start_bracket_kind = char_to_bracket_kind(current_char).unwrap();

    let mut sub_chunks: Vec<Chunk> = Vec::new();
    let mut my_state: ParseState = ParseState::Incomplete;
    let mut end_bracket_kind: Option<BracketKind> = None;
    let mut current_index = start_index + 1;
    while current_index < end {
        let (chunk, new_index) = parse_one(s, Some(current_index));
        if chunk.is_none() {
            if new_index == end {
                my_state = ParseState::Incomplete;
                current_index = new_index;
            } else {
                end_bracket_kind = char_to_bracket_kind(s[new_index]);
                if end_bracket_kind.is_none()
                    || *end_bracket_kind.as_ref().unwrap() != start_bracket_kind
                {
                    my_state = ParseState::Corrupted;
                } else {
                    my_state = ParseState::Valid;
                }
                current_index = new_index + 1;
            }
            break;
        } else {
            sub_chunks.push(chunk.unwrap());
            current_index = new_index;
        }
    }

    let chunk = Chunk {
        start_kind: start_bracket_kind,
        end_kind: end_bracket_kind,
        sub_chunks,
        state: my_state,
    };
    (Some(chunk), current_index)
}

#[derive(Debug, Clone)]
struct Chunk {
    start_kind: BracketKind,
    end_kind: Option<BracketKind>,
    sub_chunks: Vec<Chunk>,
    state: ParseState,
}

fn char_to_bracket_kind(c: char) -> Option<BracketKind> {
    match c {
        '(' | ')' => Some(BracketKind::Round),
        '[' | ']' => Some(BracketKind::Square),
        '<' | '>' => Some(BracketKind::Angled),
        '{' | '}' => Some(BracketKind::Curly),
        _ => None,
    }
}

fn bracket_kind_to_closing_char(bk: &BracketKind) -> char {
    match bk {
        BracketKind::Round => ')',
        BracketKind::Square => ']',
        BracketKind::Angled => '>',
        BracketKind::Curly => '}',
    }
}

#[derive(PartialEq, Debug, Clone)]
enum BracketKind {
    Round,
    Square,
    Angled,
    Curly,
}

#[derive(PartialEq, Debug, Clone)]
enum ParseState {
    Incomplete,
    Corrupted,
    Valid,
}

#[cfg(test)]
mod tests {
    use crate::day10::part1;
    use crate::day10::part2;
    use crate::day10::read_input;
    use crate::utils;

    #[test]
    fn part1_example() {
        let lines = read_input(utils::InputType::Example);
        assert_eq!(part1(&lines), 26397);
    }

    #[test]
    fn part1_real() {
        let lines = read_input(utils::InputType::Main);
        assert_eq!(part1(&lines), 243939);
    }

    #[test]
    fn part2_example() {
        let lines = read_input(utils::InputType::Example);
        assert_eq!(part2(&lines), 288957);
    }

    #[test]
    fn part2_real() {
        let lines = read_input(utils::InputType::Main);
        assert_eq!(part2(&lines), 2421222841);
    }
}
