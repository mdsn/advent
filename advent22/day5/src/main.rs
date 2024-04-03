use std::io;
use std::io::prelude::*;

#[derive(Debug)]
// n from x to y
struct Op(u32, usize, usize);

impl From<&String> for Op {
    fn from(s: &String) -> Self {
        let n: Vec<u32> = s
            .split(' ')
            .filter_map(|item| item.parse::<u32>().ok())
            .collect();
        Op(n[0], n[1] as usize, n[2] as usize)
    }
}

fn exec(stacks: &mut [String], op: Op) {
    for _ in 0..op.0 {
        let c = stacks[op.1 - 1].pop().unwrap();
        stacks[op.2 - 1].push(c);
    }
}

fn exec2(stacks: &mut [String], op: Op) {
    let from = &mut stacks[op.1 - 1];
    let crates = from.split_off(from.len() - op.0 as usize);
    stacks[op.2 - 1].push_str(&crates);
}

fn transpose(rows: &[String]) -> Vec<String> {
    let mut v = vec![];
    for i in 0..rows[0].len() {
        let stack: String = rows
            .iter()
            .rev()
            .filter_map(|r| match r.chars().nth(i).unwrap() {
                ' ' => None,
                c => Some(c),
            })
            .collect();
        v.push(stack);
    }
    v
}

fn main() -> io::Result<()> {
    let mut handle = io::stdin().lock();
    let mut row = String::new();

    // parse stacks initial state
    let mut rows = vec![];
    loop {
        let n = handle.read_line(&mut row)?;
        if n == 1 {
            break;
        }
        // [D] [W] [W] [F] [T] [H] [Z] [W] [R]
        //  1   5   9  13  17  21  25  29  33
        let mut parsed = String::new();
        for i in (1..row.len()).step_by(4) {
            parsed.push(row.chars().nth(i).unwrap());
        }

        rows.push(parsed);
        row.clear();
    }
    rows.pop();
    let mut stacks = transpose(&rows);

    // execute moves
    for line in handle.lines() {
        let op = Op::from(&line?);
        exec2(&mut stacks, op);
    }

    let top_crates: String = stacks
        .iter()
        .map(|s| match s {
            _ if s.is_empty() => ' ',
            _ => s.chars().nth(s.len() - 1).unwrap(),
        })
        .collect();
    println!("{top_crates}");
    Ok(())
}
