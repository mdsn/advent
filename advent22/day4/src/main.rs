use std::io;
use std::io::prelude::*;

fn contains(a: &[u32], b: &[u32]) -> bool {
    a[0] <= b[0] && b[1] <= a[1] || b[0] <= a[0] && a[1] <= b[1]
}

fn overlaps(a: &[u32], b: &[u32]) -> bool {
    let between = |x, a, b| a <= x && x <= b;
    between(a[0], b[0], b[1])
        || between(a[1], b[0], b[1])
        || between(b[0], a[0], a[1])
        || between(b[1], a[0], a[1])
}

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut contained = 0u32;
    let mut overlapping = 0u32;

    for line in stdin.lock().lines() {
        let line = line?;
        let pair: Vec<Vec<u32>> = line
            .split(',')
            .map(|p| p.split('-').map(|n| n.parse::<u32>().unwrap()).collect())
            .collect();
        if overlaps(&pair[0], &pair[1]) {
            overlapping += 1;
            if contains(&pair[0], &pair[1]) {
                contained += 1;
            }
        }
    }

    println!("{contained}");
    println!("{overlapping}");
    Ok(())
}
