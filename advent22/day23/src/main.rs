use std::collections::{HashMap, HashSet, VecDeque};
use std::io::prelude::*;
use std::ops::Add;

#[derive(Debug, Clone, Copy)]
enum Dir {
    N,
    S,
    E,
    W,
}

impl Add<Dir> for (i32, i32) {
    type Output = Self;
    fn add(self, rhs: Dir) -> Self {
        let v = match rhs {
            Dir::N => (0, -1),
            Dir::S => (0, 1),
            Dir::E => (1, 0),
            Dir::W => (-1, 0),
        };
        (self.0 + v.0, self.1 + v.1)
    }
}

fn adjacent_positions(pos: (i32, i32)) -> Vec<(i32, i32)> {
    [
        (-1, -1),
        (0, -1),
        (1, -1),
        (-1, 0),
        (1, 0),
        (-1, 1),
        (0, 1),
        (1, 1),
    ]
    .iter()
    .map(|(x, y)| (pos.0 + *x, pos.1 + *y))
    .collect()
}

fn adjacent3(pos: (i32, i32), dir: Dir) -> [(i32, i32); 3] {
    let mut p3 = match dir {
        Dir::N => [(-1, -1), (0, -1), (1, -1)],
        Dir::S => [(-1, 1), (0, 1), (1, 1)],
        Dir::E => [(1, -1), (1, 0), (1, 1)],
        Dir::W => [(-1, -1), (-1, 0), (-1, 1)],
    };

    p3.iter_mut().for_each(|p| {
        p.0 += pos.0;
        p.1 += pos.1;
    });

    p3
}

fn main() {
    let mut proposal_order = VecDeque::from([Dir::N, Dir::S, Dir::W, Dir::E]);
    let mut elves = HashMap::new();
    let mut i = 0;
    for (y, line) in std::io::stdin()
        .lock()
        .lines()
        .map(Result::unwrap)
        .enumerate()
    {
        for (x, c) in line.chars().enumerate() {
            if c == '#' {
                elves.insert(i, (x as i32, y as i32));
                i += 1;
            }
        }
    }

    for round in 1.. {
        let mut moved = false;
        let mut target_positions: HashMap<(i32, i32), Vec<i32>> = [].into();
        let current_positions = elves.values().collect::<HashSet<_>>();
        for (id, pos) in &elves {
            if adjacent_positions(*pos)
                .iter()
                .all(|p| !current_positions.contains(p))
            {
                continue;
            }

            for proposed_dir in &proposal_order {
                let adjacent = adjacent3(*pos, *proposed_dir);
                if adjacent.iter().all(|p| !current_positions.contains(p)) {
                    let target_dir = *pos + *proposed_dir;
                    target_positions.entry(target_dir).or_default().push(*id);
                    break;
                }
            }
        }

        for (target, ids) in target_positions {
            if ids.len() > 1 {
                continue;
            }
            elves.insert(ids[0], target);
            moved = true;
        }

        if round == 10 {
            let part1 = total_area(&elves) - elves.len();
            dbg!(part1);
        }

        if !moved {
            dbg!(round);
            break;
        }

        proposal_order.rotate_left(1);
    }
}

fn boundaries(elves: &HashMap<i32, (i32, i32)>) -> [i32; 4] {
    let mut b = [0i32; 4];
    b[0] = elves.values().map(|p| p.0).min().unwrap();
    b[1] = elves.values().map(|p| p.1).min().unwrap();
    b[2] = elves.values().map(|p| p.0).max().unwrap();
    b[3] = elves.values().map(|p| p.1).max().unwrap();
    b
}

fn total_area(elves: &HashMap<i32, (i32, i32)>) -> usize {
    let [minx, miny, maxx, maxy] = boundaries(elves);
    let (w, h) = ((maxx - minx).abs() + 1, (maxy - miny).abs() + 1);
    (w * h) as usize
}

#[allow(dead_code)]
fn print(elves: &HashMap<i32, (i32, i32)>) {
    let [minx, miny, maxx, maxy] = boundaries(elves);
    let positions = elves.values().collect::<Vec<_>>();
    for y in miny..=maxy {
        for x in minx..=maxx {
            print!(
                "{}",
                if positions.contains(&&(x, y)) {
                    '#'
                } else {
                    '.'
                }
            );
        }
        println!();
    }
}
