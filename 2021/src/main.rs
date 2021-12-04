extern crate clap;

mod day01;
mod day02;
mod day03;
mod day04;
mod utils;

use std::collections::HashMap;
use std::collections::HashSet;

fn main() {
    let mut day_to_fn: HashMap<String, fn() -> ()> = HashMap::new();
    day_to_fn.insert("1".to_string(), day01::run);
    day_to_fn.insert("2".to_string(), day02::run);
    day_to_fn.insert("3".to_string(), day03::run);
    day_to_fn.insert("4".to_string(), day04::run);

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
        .get_matches();

    let day_runner = day_to_fn.get(args.value_of("day").unwrap()).unwrap();
    day_runner();
}

fn validate_day(val: String) -> Result<(), String> {
    // FIXME it would be nice if I didn't have to redefine the valid days here
    let mut valid_days = HashSet::new();
    valid_days.insert("1");
    valid_days.insert("2");
    valid_days.insert("3");
    valid_days.insert("4");

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
