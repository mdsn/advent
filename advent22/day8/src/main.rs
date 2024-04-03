use std::error::Error;
use std::io;
use std::io::prelude::*;
use take_until::TakeUntilExt;

fn main() -> Result<(), Box<dyn Error>> {
    let handle = io::stdin().lock();
    let mut map: Vec<Vec<u32>> = vec![];

    for line in handle.lines() {
        let line = line?
            .chars()
            .map(|c| c.to_digit(10).unwrap())
            .collect::<Vec<_>>();
        map.push(line);
    }

    let width = map[0].len();
    let height = map.len();
    let guaranteed = 2 * (width + height) as u32 - 4;

    let mut total = 0u32;
    for x in 1..width - 1 {
        for y in 1..height - 1 {
            let tree = map[y][x];
            if (0..x).all(|i| map[y][i] < tree)
                || (x + 1..width).all(|i| map[y][i] < tree)
                || (0..y).all(|i| map[i][x] < tree)
                || (y + 1..height).all(|i| map[i][x] < tree)
            {
                total += 1;
            }
        }
    }

    let part1 = total + guaranteed;
    dbg!(part1);

    let mut part2 = 0u32;
    for x in 1..width - 1 {
        for y in 1..height - 1 {
            let tree = map[y][x];
            let left = (0..x).rev().take_until(|i| map[y][*i] >= tree).count() as u32;
            let right = (x + 1..width).take_until(|i| map[y][*i] >= tree).count() as u32;
            let top = (0..y).rev().take_until(|i| map[*i][x] >= tree).count() as u32;
            let bottom = (y + 1..height).take_until(|i| map[*i][x] >= tree).count() as u32;

            part2 = part2.max(left * right * top * bottom);
        }
    }

    dbg!(part2);
    Ok(())
}
