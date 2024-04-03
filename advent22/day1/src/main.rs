use std::collections::BinaryHeap;
use std::io;
use std::io::prelude::*;

fn main() -> io::Result<()> {
    let stdin = io::stdin();

    let mut elves = BinaryHeap::new();
    let mut total = 0u32;
    for line in stdin.lock().lines() {
        let line = line?;
        if line.is_empty() {
            elves.push(total);
            total = 0;
        } else {
            let calories = line.parse::<u32>().unwrap();
            total += calories;
        }
    }

    println!("Max calories: {}", elves.peek().unwrap());
    let top3: u32 = (0..3).map(|_| elves.pop().unwrap()).sum();
    println!("Top 3 max calories: {}", top3);
    Ok(())
}
