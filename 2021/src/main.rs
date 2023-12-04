extern crate clap;

mod utils;

mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;
mod day18;
mod day19;
mod day20;
mod day21;

use std::collections::HashMap;

const DAYS: [&str; 25] = [
    "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17",
    "18", "19", "20", "21", "22", "23", "24", "25",
];

fn main() {
    let mut day_to_fn: HashMap<String, fn(utils::InputType) -> ()> = HashMap::new();
    day_to_fn.insert("1".to_string(), day01::run);
    day_to_fn.insert("2".to_string(), day02::run);
    day_to_fn.insert("3".to_string(), day03::run);
    day_to_fn.insert("4".to_string(), day04::run);
    day_to_fn.insert("5".to_string(), day05::run);
    day_to_fn.insert("6".to_string(), day06::run);
    day_to_fn.insert("7".to_string(), day07::run);
    day_to_fn.insert("8".to_string(), day08::run);
    day_to_fn.insert("9".to_string(), day09::run);
    day_to_fn.insert("10".to_string(), day10::run);
    day_to_fn.insert("11".to_string(), day11::run);
    day_to_fn.insert("12".to_string(), day12::run);
    day_to_fn.insert("13".to_string(), day13::run);
    day_to_fn.insert("14".to_string(), day14::run);
    day_to_fn.insert("15".to_string(), day15::run);
    day_to_fn.insert("16".to_string(), day16::run);
    day_to_fn.insert("17".to_string(), day17::run);
    day_to_fn.insert("18".to_string(), day18::run);
    day_to_fn.insert("19".to_string(), day19::run);
    day_to_fn.insert("20".to_string(), day20::run);
    day_to_fn.insert("21".to_string(), day21::run);

    let args = clap::App::new("Advent of Code 2021")
        .arg(
            clap::Arg::with_name("day")
                .help("Number of the day to run")
                .takes_value(true)
                .short("d")
                .long("day")
                .validator(validate_day),
        )
        .arg(
            clap::Arg::with_name("example")
                .help("Run the example")
                .short("e")
                .long("example"),
        )
        .get_matches();

    let it = if args.is_present("example") {
        utils::InputType::Example
    } else {
        utils::InputType::Main
    };

    if args.is_present("day") {
        let day_runner = day_to_fn.get(args.value_of("day").unwrap()).unwrap();
        day_runner(it);
    } else {
        for day in DAYS {
            if day_to_fn.contains_key(day) {
                println!("Day {}:", day);
                let day_runner = day_to_fn.get(day).unwrap();
                day_runner(it);
                println!("-----------");
            }
        }
    }
}

fn validate_day(val: String) -> Result<(), String> {
    if DAYS.contains(&val.as_ref()) {
        Ok(())
    } else {
        let mut e = String::new();
        e.push_str("Invalid day. valid values are: ");
        e.push_str(&DAYS.join(" "));
        Err(e)
    }
}
