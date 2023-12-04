use core::fmt;
use std::fs::{read_to_string, File};
use std::io::{self, BufRead};

pub fn parse_file<T>(d: Day, it: InputType, f: fn(&str) -> T) -> Vec<T> {
    let filename = filename_from_day_and_input_type(d, it);
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    let mut res: Vec<T> = Vec::new();
    for line in lines {
        res.push(f(&line.unwrap()));
    }
    res
}

pub fn read_file(d: Day, it: InputType) -> String {
    let filename = filename_from_day_and_input_type(d, it);
    read_to_string(filename).unwrap()
}

fn filename_from_day_and_input_type(d: Day, it: InputType) -> String {
    let example_str = match it {
        InputType::Main => "",
        InputType::Example => "-example",
    };
    format!("resource/day-{}{}-input.txt", d, example_str)
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, clap::ValueEnum)]
#[repr(u8)]
pub enum Day {
    Day01,
    Day02,
    Day03,
    Day04,
    Day05,
    Day06,
    Day07,
    Day08,
    Day09,
    Day10,
    Day11,
    Day12,
    Day13,
    Day14,
    Day15,
    Day16,
    Day17,
    Day18,
    Day19,
    Day20,
    Day21,
    Day22,
    Day23,
    Day24,
    Day25,
}

impl fmt::Display for Day {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Day::Day01 => "01",
                Day::Day02 => "02",
                Day::Day03 => "03",
                Day::Day04 => "04",
                Day::Day05 => "05",
                Day::Day06 => "06",
                Day::Day07 => "07",
                Day::Day08 => "08",
                Day::Day09 => "09",
                Day::Day10 => "10",
                Day::Day11 => "11",
                Day::Day12 => "12",
                Day::Day13 => "13",
                Day::Day14 => "14",
                Day::Day15 => "15",
                Day::Day16 => "16",
                Day::Day17 => "17",
                Day::Day18 => "18",
                Day::Day19 => "19",
                Day::Day20 => "20",
                Day::Day21 => "21",
                Day::Day22 => "22",
                Day::Day23 => "23",
                Day::Day24 => "24",
                Day::Day25 => "25",
            }
        )
    }
}

pub const DAYS: [Day; 25] = [
    Day::Day01,
    Day::Day02,
    Day::Day03,
    Day::Day04,
    Day::Day05,
    Day::Day06,
    Day::Day07,
    Day::Day08,
    Day::Day09,
    Day::Day10,
    Day::Day11,
    Day::Day12,
    Day::Day13,
    Day::Day14,
    Day::Day15,
    Day::Day16,
    Day::Day17,
    Day::Day18,
    Day::Day19,
    Day::Day20,
    Day::Day21,
    Day::Day22,
    Day::Day23,
    Day::Day24,
    Day::Day25,
];

#[derive(Copy, Clone)]
pub enum InputType {
    Main,
    Example,
}

#[macro_export]
macro_rules! time {
    ($context: literal, $s: stmt) => {
        let timer = std::time::Instant::now();
        $s
        println!("{} took: {:?}\n", $context, timer.elapsed())
    }
}
