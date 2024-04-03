#![feature(iter_intersperse)]

use std::collections::HashMap;
use std::dbg;
use std::error::Error;
use std::io;
use std::io::prelude::*;

const LIMIT: u32 = 100000;
const NEEDED: u32 = 70000000 - 30000000;

fn make_path(components: &[String]) -> String {
    components
        .iter()
        .cloned()
        .intersperse("/".to_string())
        .collect()
}

struct Prefixes {
    i: usize,
    components: Vec<String>,
}

impl Iterator for Prefixes {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if self.i <= self.components.len() {
            let path = make_path(&self.components[..self.i]);
            self.i += 1;
            return Some(path);
        }
        None
    }
}

fn prefixes(components: &[String]) -> Prefixes {
    Prefixes {
        i: 0,
        components: components.to_vec(),
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let handle = io::stdin().lock();
    let mut sizes: HashMap<String, u32> = HashMap::new();
    let mut cwd: Vec<String> = vec![];

    for line in handle.lines() {
        let line = line?;
        let tokens = line.split(' ').collect::<Vec<_>>();
        match tokens[0] {
            "$" => match tokens[1] {
                "cd" => match tokens[2] {
                    "/" => {}
                    ".." => {
                        cwd.pop();
                    }
                    _ => {
                        cwd.push(tokens[2].to_string());
                    }
                },
                "ls" => {}
                _ => unreachable!(),
            },
            "dir" => {}
            _ => {
                let size = str::parse::<u32>(tokens[0]).unwrap();
                for prefix in prefixes(&cwd) {
                    *sizes.entry(prefix).or_default() += size;
                }
            }
        }
    }

    let part1: u32 = sizes.values().filter(|sz| **sz <= LIMIT).sum();
    dbg!(part1);

    let used = *sizes.get("").unwrap();
    let mut sizes: Vec<u32> = sizes
        .into_values()
        .filter(|sz| *sz >= used - NEEDED)
        .collect();
    sizes.sort();
    let part2 = sizes[0];
    dbg!(part2);

    Ok(())
}
