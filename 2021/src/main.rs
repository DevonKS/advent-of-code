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

use std::collections::HashMap;
use std::collections::HashSet;

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

    let args = clap::App::new("Advent of Code 2021")
        .arg(
            clap::Arg::with_name("day")
                .help("Number of the day to run")
                .takes_value(true)
                .short("d")
                .long("day")
                .required(true)
                .validator(validate_day),
        )
        .arg(
            clap::Arg::with_name("example")
                .help("Run the example")
                .short("e")
                .long("example"),
        )
        .get_matches();

    let day_runner = day_to_fn.get(args.value_of("day").unwrap()).unwrap();
    let it = if args.is_present("example") {
        utils::InputType::Example
    } else {
        utils::InputType::Main
    };
    day_runner(it);
}

fn validate_day(val: String) -> Result<(), String> {
    // FIXME it would be nice if I didn't have to redefine the valid days here
    let mut valid_days = HashSet::new();
    valid_days.insert("1");
    valid_days.insert("2");
    valid_days.insert("3");
    valid_days.insert("4");
    valid_days.insert("5");
    valid_days.insert("6");
    valid_days.insert("7");
    valid_days.insert("8");
    valid_days.insert("9");
    valid_days.insert("10");
    valid_days.insert("11");
    valid_days.insert("12");

    if valid_days.contains(&val as &str) {
        Ok(())
    } else {
        let mut e = String::new();
        e.push_str("Invalid day. valid values are: ");
        let mut v = valid_days.into_iter().collect::<Vec<&str>>();
        v.sort();
        e.push_str(&v.join(" "));
        Err(e)
    }
}
