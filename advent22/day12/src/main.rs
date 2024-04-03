use std::collections::VecDeque;
use std::error::Error;
use std::io;
use std::io::prelude::*;

type Map = Vec<Vec<u8>>;

fn tile_height(c: char) -> u8 {
    c as u8 - 97
}

fn shortest_path(start: (usize, usize), end: (usize, usize), map: &Map) -> Option<u32> {
    let map_w = map[0].len();
    let map_h = map.len();

    let mut visited = vec![vec![false; map[0].len()]; map.len()];
    visited[start.0][start.1] = true;

    let mut q = VecDeque::new();
    q.push_back((start, 0));

    while let Some(((y, x), d)) = q.pop_front() {
        if (y, x) == end {
            return Some(d);
        }

        for (dy, dx) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
            if dy < 0 && y == 0
                || dy > 0 && y == map_h - 1
                || dx < 0 && x == 0
                || dx > 0 && x == map_w - 1
            {
                continue;
            }

            let y1 = (y as i32 + dy) as usize;
            let x1 = (x as i32 + dx) as usize;

            if map[y1][x1] as i32 - map[y][x] as i32 <= 1 && !visited[y1][x1] {
                visited[y1][x1] = true;
                q.push_back(((y1, x1), d + 1));
            }
        }
    }

    None
}

fn main() -> Result<(), Box<dyn Error>> {
    let handle = io::stdin().lock();

    let mut start = (0, 0);
    let mut end = (0, 0);

    let mut map: Map = vec![];
    for line in handle.lines() {
        let line: Vec<u8> = line?
            .chars()
            .enumerate()
            .map(|(i, c)| match c {
                'S' => {
                    start = (map.len(), i);
                    0u8
                }
                'E' => {
                    end = (map.len(), i);
                    tile_height('z')
                }
                _ => tile_height(c),
            })
            .collect();
        map.push(line);
    }

    if let Some(distance) = shortest_path(start, end, &map) {
        dbg!(distance);
    }

    let mut shortest = u32::MAX;
    for y in 0..map.len() {
        for x in 0..map[0].len() {
            if map[y][x] == 0 {
                if let Some(distance) = shortest_path((y, x), end, &map) {
                    shortest = shortest.min(distance);
                }
            }
        }
    }
    dbg!(shortest);

    Ok(())
}
