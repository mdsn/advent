use std::collections::HashSet;
use std::error::Error;
use std::io;
use std::io::prelude::*;
use std::ops::AddAssign;

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
struct P {
    x: i32,
    y: i32,
}

impl P {
    fn new() -> Self {
        P { x: 0, y: 0 }
    }
}

// a covering b counts as adjacent
fn adjacent(a: &P, b: &P) -> bool {
    (a.x - b.x).abs() <= 1 && (a.y - b.y).abs() <= 1
}

impl AddAssign<(i32, i32)> for P {
    fn add_assign(&mut self, rhs: (i32, i32)) {
        self.x += rhs.0;
        self.y += rhs.1;
    }
}

type D = char;

fn parse(cmd: &str) -> (D, i32) {
    let parts: Vec<_> = cmd.split(' ').collect();
    (
        parts[0].chars().next().unwrap(),
        parts[1].parse::<i32>().unwrap(),
    )
}

fn pull_rope(rope: &mut [P], d: &D) {
    rope[0] += match d {
        'L' => (-1, 0),
        'R' => (1, 0),
        'U' => (0, 1),
        'D' => (0, -1),
        _ => unreachable!(),
    };
    for i in 0..rope.len() - 1 {
        let (h, t) = rope.split_at_mut(i + 1);
        let h = &h[i];
        let t = &mut t[0];
        if !adjacent(h, t) {
            let (dx, dy) = (h.x - t.x, h.y - t.y);
            *t += (dx.signum(), dy.signum());
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let handle = io::stdin().lock();

    let mut moves: Vec<(D, i32)> = vec![];
    for line in handle.lines() {
        let line = line?;
        moves.push(parse(&line));
    }

    let mut ropes = [vec![P::new(); 2], vec![P::new(); 10]];
    let mut visited: HashSet<P> = HashSet::new();

    for rope in ropes.iter_mut() {
        for (d, n) in &moves {
            for _ in 0..*n {
                pull_rope(rope, d);
                visited.insert(rope[rope.len() - 1]);
            }
        }
        dbg!(visited.len());
        visited.clear();
    }

    Ok(())
}
