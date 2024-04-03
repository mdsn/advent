use std::io;
use std::io::prelude::*;

#[derive(Copy, Clone)]
enum Rps {
    Rock = 1,
    Paper = 2,
    Scissors = 3,
}

impl From<char> for Rps {
    fn from(c: char) -> Self {
        match c {
            'A' => Rps::Rock,
            'B' => Rps::Paper,
            'C' => Rps::Scissors,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy)]
enum Result {
    Loss = 0,
    Tie = 3,
    Win = 6,
}

impl From<char> for Result {
    fn from(c: char) -> Self {
        match c {
            'X' => Result::Loss,
            'Y' => Result::Tie,
            'Z' => Result::Win,
            _ => unreachable!(),
        }
    }
}

fn determine_hand(opponent: &Rps, result: &Result) -> Rps {
    match result {
        Result::Win => match opponent {
            Rps::Rock => Rps::Paper,
            Rps::Paper => Rps::Scissors,
            Rps::Scissors => Rps::Rock,
        },
        Result::Loss => match opponent {
            Rps::Rock => Rps::Scissors,
            Rps::Paper => Rps::Rock,
            Rps::Scissors => Rps::Paper,
        },
        Result::Tie => *opponent,
    }
}

fn main() {
    let stdin = io::stdin();
    let total: u32 = stdin
        .lock()
        .lines()
        .map(io::Result::unwrap)
        .map(|line| {
            let opponent = Rps::from(line.chars().next().unwrap());
            let result = Result::from(line.chars().nth(2).unwrap());
            let own = determine_hand(&opponent, &result);
            (own as u32) + (result as u32)
        })
        .sum();
    println!("{}", total);
}
