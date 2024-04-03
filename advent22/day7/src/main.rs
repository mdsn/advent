use std::dbg;
use std::error::Error;
use std::io;
use std::io::prelude::*;

const LIMIT: u32 = 100000;
const NEEDED: u32 = 3956976; // cheat for coercion to fn

#[derive(Debug)]
struct Dir {
    size: u32,
    subd: Vec<Dir>,
}

impl Dir {
    fn new() -> Self {
        Dir {
            size: 0,
            subd: vec![],
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let handle = io::stdin().lock();
    let mut stack: Vec<Dir> = vec![];
    let mut current = Dir::new();

    for line in handle.lines() {
        let line = line?;
        let tokens = line.split(' ').collect::<Vec<_>>();
        match tokens[0] {
            "$" => match tokens[1] {
                "cd" => match tokens[2] {
                    "/" => {}
                    ".." => {
                        let mut parent = stack.pop().unwrap();
                        parent.subd.push(current);
                        current = parent;
                    }
                    _ => {
                        stack.push(current);
                        current = Dir::new();
                    }
                },
                "ls" => {}
                _ => unreachable!(),
            },
            "dir" => {}
            _ => {
                current.size += str::parse::<u32>(tokens[0]).unwrap();
            }
        }
    }

    while !stack.is_empty() {
        let mut parent = stack.pop().unwrap();
        parent.subd.push(current);
        current = parent;
    }
    let root = current;

    let (_, sizes) = dive(&root, |sz| sz <= LIMIT);
    let part1: u32 = sizes.iter().sum();
    dbg!(part1);

    let (_, mut sizes) = dive(&root, |sz| sz >= NEEDED);
    sizes.sort();
    let part2 = &sizes[0];
    dbg!(part2);

    Ok(())
}

fn dive(dir: &Dir, pred: fn(u32) -> bool) -> (u32, Vec<u32>) {
    let mut total = dir.size;
    let mut sizes = vec![];
    for subd in &dir.subd {
        let (subtotal, subsizes) = dive(subd, pred);
        total += subtotal;
        sizes.extend(subsizes.into_iter().filter(|s| pred(*s)));
    }
    if pred(total) {
        sizes.push(total);
    }
    (total, sizes)
}
