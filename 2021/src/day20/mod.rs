use std::collections::HashMap;

use crate::{time, utils};

pub fn run(it: utils::InputType) {
    let (alg, image) = read_input(it);
    time!("Part 1", println!("{}", part1(&alg, &image)));
    time!("Part 2", println!("{}", part2(&alg, &image)));
}

fn part2(alg: &[bool], image: &Image) -> usize {
    let mut img = image.clone();
    for _ in 0..50 {
        img = enhance_image(alg, &img);
    }
    img.len()
}

fn part1(alg: &[bool], image: &Image) -> usize {
    let mut img = image.clone();
    for _ in 0..2 {
        img = enhance_image(alg, &img);
    }
    img.len()
}

fn enhance_image(alg: &[bool], image: &Image) -> Image {
    let mut new_image = Image::new();
    for r in image.min_r - 1..image.max_r + 2 {
        for c in image.min_c - 1..image.max_c + 2 {
            let idx = image.count_around_point(r, c);
            new_image.insert(r, c, alg[idx]);
        }
    }
    new_image.is_infinite = image.is_infinite;
    new_image.set_is_infinite(alg);
    new_image
}

fn read_input(it: utils::InputType) -> (Vec<bool>, Image) {
    let input_str = utils::read_file(utils::Day::Day20, it);
    let parts: Vec<&str> = input_str.split("\n\n").collect();
    assert_eq!(2, parts.len());

    let alg = parts[0]
        .trim()
        .chars()
        .map(|c| match c {
            '.' => false,
            '#' => true,
            _ => panic!("Unexpected character when parsing image alg: {}", c),
        })
        .collect();

    let mut image = Image::new();
    for (r, row) in parts[1].lines().enumerate() {
        for (c, character) in row.chars().enumerate() {
            let mut val = false;
            if character == '#' {
                val = true;
            }
            image.insert(
                isize::try_from(r).unwrap(),
                isize::try_from(c).unwrap(),
                val,
            );
        }
    }

    (alg, image)
}

#[derive(Clone, Debug)]
struct Image {
    points: HashMap<(isize, isize), bool>,
    min_r: isize,
    max_r: isize,
    min_c: isize,
    max_c: isize,
    is_infinite: bool,
}

impl Image {
    fn new() -> Image {
        Image {
            points: HashMap::new(),
            min_r: 0,
            max_r: 0,
            min_c: 0,
            max_c: 0,
            is_infinite: false,
        }
    }

    fn len(&self) -> usize {
        if self.is_infinite {
            panic!("Cannot get the length of an infinite image");
        } else {
            return self.points.iter().filter(|item| *item.1).count();
        }
    }

    fn insert(&mut self, r: isize, c: isize, val: bool) {
        self.points.insert((r, c), val);

        if r < self.min_r {
            self.min_r = r
        } else if r > self.max_r {
            self.max_r = r
        }

        if c < self.min_c {
            self.min_c = c
        } else if c > self.max_c {
            self.max_c = c
        }
    }

    #[allow(dead_code)]
    fn draw(&self, min_r: isize, max_r: isize, min_c: isize, max_c: isize) -> String {
        let mut s = String::new();
        for r in min_r..max_r + 1 {
            for c in min_c..max_c + 1 {
                if self.points.contains_key(&(r, c)) {
                    s.push('#');
                } else {
                    s.push('.');
                }
            }
            s.push('\n');
        }
        s
    }

    fn count_around_point(&self, r: isize, c: isize) -> usize {
        let mut num_str = String::new();
        for delta_r in [-1, 0, 1] {
            for delta_c in [-1, 0, 1] {
                let x = self
                    .points
                    .get(&(r + delta_r, c + delta_c))
                    .unwrap_or(&self.is_infinite);
                let character = if *x { '1' } else { '0' };
                num_str.push(character)
            }
        }
        usize::from_str_radix(&num_str, 2).unwrap()
    }

    fn set_is_infinite(&mut self, alg: &[bool]) {
        if self.is_infinite {
            self.is_infinite = alg[alg.len() - 1];
        } else {
            self.is_infinite = alg[0];
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        day20::{part1, part2, read_input},
        utils,
    };

    #[test]
    fn part1_example() {
        let (alg, image) = read_input(utils::InputType::Example);
        assert_eq!(part1(&alg, &image), 35);
    }

    #[test]
    fn part1_real() {
        let (alg, image) = read_input(utils::InputType::Main);
        assert_eq!(part1(&alg, &image), 5483);
    }

    #[test]
    fn part2_example() {
        let (alg, image) = read_input(utils::InputType::Example);
        assert_eq!(part2(&alg, &image), 3351);
    }

    #[test]
    #[ignore]
    fn part2_real() {
        let (alg, image) = read_input(utils::InputType::Main);
        assert_eq!(part2(&alg, &image), 18732);
    }
}
