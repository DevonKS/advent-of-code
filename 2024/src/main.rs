extern crate clap;

use clap::Parser;

mod util;

mod day01;
mod day02;
mod day03;
mod day04;

use std::collections::HashMap;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(value_enum, short, long)]
    day: Option<util::Day>,

    #[arg(short, long)]
    example: bool,
}

fn main() {
    let mut day_to_fn: HashMap<util::Day, fn(util::InputType) -> ()> = HashMap::new();
    day_to_fn.insert(util::Day::Day01, day01::run);
    day_to_fn.insert(util::Day::Day02, day02::run);
    day_to_fn.insert(util::Day::Day03, day03::run);
    day_to_fn.insert(util::Day::Day04, day04::run);

    let args = Args::parse();

    let it = match args.example {
        true => util::InputType::Example,
        false => util::InputType::Main,
    };

    match args.day {
        Some(d) => {
            let day_runner = day_to_fn.get(&d).unwrap();
            day_runner(it);
        }
        None => {
            for day in util::DAYS {
                if day_to_fn.contains_key(&day) {
                    println!("Day {}:", day);
                    let day_runner = day_to_fn.get(&day).unwrap();
                    day_runner(it);
                    println!("-----------");
                }
            }
        }
    }
}
