#![feature(iter_array_chunks)]

use std::collections::HashSet;
use std::io;
use std::io::prelude::*;

const ITEMS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

fn priority(c: char) -> u32 {
    ITEMS.find(c).unwrap() as u32 + 1
}

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut total = 0u32;

    for group in stdin.lock().lines().array_chunks::<3>() {
        let mut group = group
            .into_iter()
            .map(|bag| HashSet::from_iter(bag.unwrap().chars()));
        let mut frist: HashSet<char> = group.next().unwrap();
        for bag in group {
            frist.retain(|c| bag.contains(c));
        }
        assert_eq!(frist.len(), 1);
        total += priority(*frist.iter().next().unwrap());
    }

    println!("{total}");
    Ok(())
}
