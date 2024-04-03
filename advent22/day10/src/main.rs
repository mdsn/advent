use std::collections::VecDeque;
use std::error::Error;
use std::io;
use std::io::prelude::*;

enum Op {
    Noop,
    Addx(i32),
}

impl Op {
    fn cycles(&self) -> u32 {
        match self {
            Op::Noop => 1,
            Op::Addx(_) => 2,
        }
    }
}

fn parse(input: &str) -> Op {
    let parts: Vec<_> = input.split(' ').collect();
    match parts[0] {
        "noop" => Op::Noop,
        "addx" => {
            let arg = parts[1].parse::<i32>().unwrap();
            Op::Addx(arg)
        }
        _ => unreachable!(),
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let handle = io::stdin().lock();

    let mut marks: VecDeque<_> = (20..).step_by(40).take(6).collect();
    let mut newlines: VecDeque<_> = (40..).step_by(40).take(6).collect();

    let mut signals = 0i32;
    let mut cycle = 0u32;
    let mut regx = 1i32;

    for line in handle.lines() {
        let op = parse(&line?);
        for _ in 0..op.cycles() {
            cycle += 1;

            print!(
                "{}",
                if (regx - ((cycle - 1) % 40) as i32).abs() <= 1 {
                    "#"
                } else {
                    "."
                }
            );

            if !newlines.is_empty() && cycle == newlines[0] {
                println!();
                newlines.pop_front();
            }

            if !marks.is_empty() && cycle == marks[0] {
                signals += regx * cycle as i32;
                marks.pop_front();
            }
        }
        if let Op::Addx(arg) = op {
            regx += arg;
        }
    }

    dbg!(signals);
    Ok(())
}
