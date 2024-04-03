use std::collections::{HashMap, HashSet, VecDeque};
use std::io;
use std::io::prelude::*;

type Pos = (i32, i32);
type Map = HashMap<Pos, char>;

const NORTH: Pos = (0, -1);
const SOUTH: Pos = (0, 1);
const EAST: Pos = (1, 0);
const WEST: Pos = (-1, 0);

fn main() {
    let mut map = HashMap::new();
    let mut start = (0, 0);
    for (y, line) in io::stdin().lock().lines().map(Result::unwrap).enumerate() {
        for (x, c) in line.chars().enumerate() {
            map.insert((x as i32, y as i32), c);
            if c == 'S' {
                start = (x as i32, y as i32);
            }
        }
    }

    // println!("{}", walk(&map, &start));

    let boundary= 20;
    let path  = HashSet::from_iter(traverse_path(&map, &start).into_iter());
    // let inside = traverse_with_normal(&map, &path, &start);
    // println!("PATH:");
    // print_set(&HashSet::from_iter(path.iter().cloned()), boundary);
    // println!("INSIDE:");
    // print_set(&inside, boundary);

     println!("OUTSIDE:");
     let outside = flood(&map, &path, boundary);
     print_set(&outside, boundary);
     let union: HashSet<Pos> = outside.union(&path).cloned().collect();
     let all_tiles = map.keys().cloned().collect::<HashSet<Pos>>();
     let inside = all_tiles.difference(&union).cloned().collect::<HashSet<Pos>>();
     println!("INSIDE:");
     print_set(&inside, boundary);

     println!("Count: {}", inside.len());
     
     // 770 too high
}

fn flood(map: &Map, path: &HashSet<Pos>, boundary: i32) -> HashSet<Pos> {
    let mut outside = HashSet::new();
    let mut queue = VecDeque::new();

    queue.push_back((0, 0));
    queue.push_back((boundary-1, boundary-1));
    while !queue.is_empty() {
        let pos = queue.pop_front().unwrap();
        outside.insert(pos);
        for n in adjacent(&pos, boundary).iter().filter(|p| {
            !outside.contains(p) && !path.contains(p)
        }) {
            if !queue.contains(n) {
                queue.push_back(*n);
            }
        }
    }

    outside
}

fn adjacent(pos: &Pos, boundary: i32) -> Vec<Pos> {
    let adjacent_tiles = [
        [0, -1], [-1, 0], [1, 0], [0, 1],
        // [-1, -1], [0, -1], [1, -1],
        // [-1, 0], [1, 0],
        // [-1, 1], [0, 1], [1, 1],
    ];
    let mut neighbors = vec![];
    for [x, y] in adjacent_tiles.iter() {
        let xn = x + pos.0;
        let yn = y + pos.1;
        if xn < 0 || xn >= boundary || yn < 0 || yn >= boundary {
            continue;
        }
        neighbors.push((xn, yn));
    }
    neighbors
}

fn traverse_path(map: &Map, start: &Pos) -> Vec<Pos> {
    let mut path = Vec::new();

    path.push(*start);
    let initial = find_neighbors(map, start);

    let mut a = start.clone();
    let mut b = select_clockwise(start, &initial);

    while b != *start {
        path.push(b);
        let bn = step(map, &a, &b);
        a = b;
        b = bn;
    }
    path
}

fn traverse_with_normal(map: &Map, path: &Vec<Pos>, start: &Pos) -> HashSet<Pos> {
    let pathset: HashSet<Pos> = HashSet::from_iter(path.iter().cloned());
    let mut inside = HashSet::new();

    let mut a = *start;
    for b in path.iter().skip(1) {
        let n = add(&b, &normal(&a, b));
        if !pathset.contains(&n) {
            inside.insert(n);
        }
        a = *b;
    }

    inside
}

fn normal(a: &Pos, b: &Pos) -> Pos {
    let (x0, y0) = a;
    let (x1, y1) = b;
    let (dx, dy) = (x1-x0, y1-y0);
    (-dy, dx)
}

fn walk(map: &Map, start: &Pos) -> i32 {
    let mut a0 = start.clone();
    let mut b0 = start.clone();

    let initial = find_neighbors(map, start);
    let mut a = initial[0];
    let mut b = initial[1];

    let mut n = 1;
    while a != b {
        let ta = a.clone();
        let tb = b.clone();
        a = step(map, &a0, &a);
        b = step(map, &b0, &b);
        a0 = ta;
        b0 = tb;
        n += 1;
    }

    n
}

fn step(map: &Map, from: &Pos, to: &Pos) -> Pos {
    find_neighbors(map, to)
        .iter()
        .filter(|p| *p != from)
        .next()
        .unwrap()
        .clone()
}


fn find_neighbors(map: &Map, pos: &Pos) -> Vec<Pos> {
    let c = map[&pos];
    let valid_dirs = match c {
        '-' => vec![EAST, WEST],
        '|' => vec![NORTH, SOUTH],
        '7' => vec![WEST, SOUTH],
        'J' => vec![WEST, NORTH],
        'L' => vec![EAST, NORTH],
        'F' => vec![EAST, SOUTH],
        'S' => vec![NORTH, SOUTH, EAST, WEST],
        _ => unreachable!(),
    };

    let mut neighbors = vec![];
    for direction in valid_dirs.iter() {
        let npos = (pos.0 + direction.0, pos.1 + direction.1);
        if let Some(neighbor) = map.get(&npos) {
            if valid_neighbor_at(direction, *neighbor) {
                neighbors.push(npos);
            }
        } else {
            continue;
        }
    }

    neighbors
}

fn valid_neighbor_at(direction: &Pos, c: char) -> bool {
    match direction {
        &NORTH => ['7', '|', 'F', 'S'].contains(&c),
        &WEST => ['L', '-', 'F', 'S'].contains(&c),
        &EAST => ['J', '-', '7', 'S'].contains(&c),
        &SOUTH => ['J', '|', 'L', 'S'].contains(&c),
        _ => unreachable!(),
    }
}

fn print_set(set: &HashSet<Pos>, boundary: i32) {
    for y in 0..boundary {
        for x in 0..boundary {
            if set.contains(&(x, y)) {
                print!("*");
            } else {
                print!(".");
            }
        }
        println!();
    }
}

fn select_clockwise(start: &Pos, neighbors: &Vec<Pos>) -> Pos {
    enum D { N, E, S, W };
    let d0 = match (neighbors[0].0 - start.0, neighbors[0].1 - start.1) {
        (1, 0) => D::E,
        (-1, 0) => D::W,
        (0, 1) => D::S,
        (0, -1) => D::N,
        _ => unreachable!(),
    };
    let d1 = match (neighbors[1].0 - start.0, neighbors[1].1 - start.1) {
        (1, 0) => D::E,
        (-1, 0) => D::W,
        (0, 1) => D::S,
        (0, -1) => D::N,
        _ => unreachable!(),
    };
    match (d0, d1) {
        (D::W, D::E) => neighbors[1],
        (D::S, D::N) => neighbors[1],
        (D::S, D::E) => neighbors[1],
        (D::W, D::S) => neighbors[1],
        (D::N, D::W) => neighbors[1],
        (D::E, D::N) => neighbors[1],

        (D::E, D::W) => neighbors[0],
        (D::N, D::S) => neighbors[0],
        (D::E, D::S) => neighbors[0],
        (D::S, D::W) => neighbors[0],
        (D::W, D::N) => neighbors[0],
        (D::N, D::E) => neighbors[0],

        _ => unreachable!(),
    }
}


fn add(a: &Pos, b: &Pos) -> Pos {
    (a.0+b.0, a.1+b.1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normal() {
        assert_eq!(normal(&(1, 1), &(2, 1)), (0, 1)); // down
        assert_eq!(normal(&(2, 1), &(2, 2)), (-1, 0)); // left
        assert_eq!(normal(&(2, 1), &(1, 1)), (0, -1)); // up
        assert_eq!(normal(&(1, 2), &(1, 1)), (1, 0)); // right
    }
}
