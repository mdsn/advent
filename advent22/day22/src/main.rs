use std::collections::HashMap;
use std::io::prelude::*;

#[derive(Debug)]
struct Map {
    tiles: HashMap<(usize, usize), char>,
    offsets: Vec<usize>,
    lengths: Vec<usize>,
}

impl Map {
    fn start(&self) -> (usize, usize) {
        let mut x = self.offsets[0];
        while self.tiles[&(0, x)] == '#' {
            x += 1;
        }
        (0, x)
    }
}

#[derive(Debug, Clone, Copy)]
enum Ins {
    Walk(usize),
    L,
    R,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Dir {
    W,
    E,
    N,
    S,
}

#[derive(Debug, PartialEq, Eq)]
struct P {
    pos: (usize, usize),
    dir: Dir,
}

impl P {
    fn south(&self) -> bool {
        matches!(self.dir, Dir::S)
    }
    fn north(&self) -> bool {
        matches!(self.dir, Dir::N)
    }
    fn west(&self) -> bool {
        matches!(self.dir, Dir::W)
    }
    fn east(&self) -> bool {
        matches!(self.dir, Dir::E)
    }
}

fn main() {
    let mut lines = std::io::stdin()
        .lock()
        .lines()
        .map(Result::unwrap)
        .collect::<Vec<_>>();
    let mut tiles: HashMap<(usize, usize), char> = [].into();
    let mut offsets = vec![];
    let mut lengths = vec![];

    let path = lines.pop().unwrap();
    lines.pop();

    for (y, line) in lines.iter().enumerate() {
        let mut x = 0;
        for c in line.chars() {
            if c != ' ' {
                break;
            }
            x += 1;
        }
        offsets.push(x);

        let line = &line[x..];
        x = 0;
        for c in line.chars() {
            tiles.insert((y, x + offsets[y]), c);
            x += 1;
        }
        lengths.push(x);
    }

    let map = Map {
        tiles,
        offsets,
        lengths,
    };

    // parse path instructions
    let mut instructions = vec![];
    for chunk in path.split_inclusive(|c: char| c.is_alphabetic()) {
        if chunk.chars().last().unwrap().is_alphabetic() {
            let (ins1, ins2) = chunk.split_at(chunk.len() - 1);
            instructions.push(Ins::Walk(ins1.parse::<usize>().unwrap()));
            instructions.push(match ins2 {
                "L" => Ins::L,
                "R" => Ins::R,
                _ => unreachable!(),
            });
        } else {
            instructions.push(Ins::Walk(chunk.parse::<usize>().unwrap()));
        }
    }

    let mut p = P {
        pos: map.start(),
        dir: Dir::E,
    };

    for ins in &instructions {
        match *ins {
            Ins::Walk(n) => walk(&map, &mut p, n),
            side => {
                p.dir = turn(p.dir, side);
            }
        }
    }

    let part1 = password(&p);
    dbg!(part1);

    let mut p = P {
        pos: map.start(),
        dir: Dir::E,
    };

    for ins in &instructions {
        match *ins {
            Ins::Walk(n) => walk_cube(&map, &mut p, n),
            side => {
                p.dir = turn(p.dir, side);
            }
        }
    }

    let part2 = password(&p);
    dbg!(part2);
}

fn password(p: &P) -> usize {
    let (y, x) = p.pos;
    1000 * (y + 1)
        + 4 * (x + 1)
        + match p.dir {
            Dir::E => 0,
            Dir::S => 1,
            Dir::W => 2,
            Dir::N => 3,
        }
}

//          +--G--+--F--+       A turns W->E
//         A|     |     |D      B turns N->E, W->S
//          +-----+--C--+       C turns E->N, S->W
//         B|     |C            D turns E->W
//    +--B--+-----+             E turns S->W, E->N
//   A|     |     |D            F turns S->S, N->N
//    +-----+--E--+             G turns W->S, N->E
//   G|     |E
//    +--F--+

fn next_pos_cube(p: &P) -> P {
    let (y, x) = p.pos;
    // A, facing W: x = 50, 0 <= y < 50
    let (y0, x0, d0) = if p.west() && x == 50 && y < 50 {
        (150 - (y + 1), 0, Dir::E)
    }
    // A, facing W: x = 0, 100 <= y <= 150
    else if p.west() && x == 0 && (100..150).contains(&y) {
        (50 - (y - 100 + 1), 50, Dir::E)
    }
    // B, facing W: x = 50, 50 <= y < 100
    else if p.west() && x == 50 && (50..100).contains(&y) {
        (100, y - 50, Dir::S)
    }
    // B, facing N: y = 100, 0 <= x < 50
    else if p.north() && y == 100 && x < 50 {
        (50 + x, 50, Dir::E)
    }
    // C, facing S: y = 49, 100 <= x < 150
    else if p.south() && y == 49 && (100..150).contains(&x) {
        (x - 50, 99, Dir::W)
    }
    // C, facing E: x = 99, 50 <= y < 100
    else if p.east() && x == 99 && (50..100).contains(&y) {
        (49, y + 50, Dir::N)
    }
    // D, facing E: x = 99, 100 <= y < 150
    else if p.east() && x == 99 && (100..150).contains(&y) {
        (50 - (y - 100 + 1), 149, Dir::W)
    }
    // D, facing E: x = 149, 0 <= y < 50
    else if p.east() && x == 149 && y < 50 {
        (150 - (y + 1), 99, Dir::W)
    }
    // E, facing S: y = 150, 50 <= x < 100
    else if p.south() && y == 149 && (50..100).contains(&x) {
        (150 + (x - 50), 49, Dir::W)
    }
    // E, facing E: x = 50, 150 <= y < 200
    else if p.east() && x == 49 && (150..200).contains(&y) {
        (149, 50 + (y - 150), Dir::N)
    }
    // F, facing N: y = 0, 100 <= x < 150
    else if p.north() && y == 0 && (100..150).contains(&x) {
        (199, x - 100, Dir::N)
    }
    // F, facing S: y = 200, 0 <= x < 50
    else if p.south() && y == 199 && x < 50 {
        (0, x + 100, Dir::S)
    }
    // G, facing W: x = 0, 150 <= y < 200
    else if p.west() && x == 0 && (150..200).contains(&y) {
        (0, 50 + (y - 150), Dir::S)
    }
    // G, facing N: y = 0, 50 <= x < 150
    else if p.north() && y == 0 && (50..150).contains(&x) {
        (150 + (x - 50), 0, Dir::E)
    } else {
        let d = match p.dir {
            Dir::W => (0, -1),
            Dir::E => (0, 1),
            Dir::N => (-1, 0),
            Dir::S => (1, 0),
        };
        ((y as i32 + d.0) as usize, (x as i32 + d.1) as usize, p.dir)
    };

    P {
        pos: (y0, x0),
        dir: d0,
    }
}

fn walk_cube(map: &Map, p: &mut P, n: usize) {
    for _ in 0..n {
        let target = next_pos_cube(p);
        if map.tiles[&target.pos] == '.' {
            *p = target;
        } else {
            return;
        }
    }
}

fn walk(map: &Map, p: &mut P, n: usize) {
    for _ in 0..n {
        let target = next_pos(map, p);
        if map.tiles[&target] == '.' {
            p.pos = target;
        } else {
            return;
        }
    }
}

fn next_pos(map: &Map, p: &P) -> (usize, usize) {
    // let (y, x) be the current position
    let (y, x) = p.pos;
    // direction maps to one of (1, 0), (-1, 0), (0, 1), (0, -1); let's call that d
    let d = match p.dir {
        Dir::W => (0, -1),
        Dir::E => (0, 1),
        Dir::N => (-1, 0),
        Dir::S => (1, 0),
    };
    // if y == 0, or (y-1, x) not in the map, and we are facing north, find the lowest row
    // with offset < x < offset + lenght
    if matches!(p.dir, Dir::N) && (y == 0 || !map.tiles.contains_key(&(y - 1, x))) {
        let mut y0 = map.offsets.len() - 1;
        while map.offsets[y0] >= x || x >= map.offsets[y0] + map.lengths[y0] {
            y0 -= 1;
        }
        (y0, x)
    }
    // if (y+1, x) is not in the map, and we are facing south, find the highest row with offset < x
    else if matches!(p.dir, Dir::S) && !map.tiles.contains_key(&(y + 1, x)) {
        let mut y0 = 0;
        while map.offsets[y0] >= x {
            y0 += 1;
        }
        (y0, x)
    }
    // if x-offset[y] == 0, and we are facing west, wrap around to offset + row_len - 1
    else if matches!(p.dir, Dir::W) && x - map.offsets[y] == 0 {
        (y, map.offsets[y] + map.lengths[y] - 1)
    }
    // if x-offset[y] = row_len[y], and we are facing east, wrap around to offset[y]
    else if matches!(p.dir, Dir::E) && x - map.offsets[y] == map.lengths[y] - 1 {
        (y, map.offsets[y])
    }
    // otherwise return (y, x) + d
    else {
        ((y as i32 + d.0) as usize, (x as i32 + d.1) as usize)
    }
}

fn turn(facing: Dir, side: Ins) -> Dir {
    assert!(!matches!(side, Ins::Walk(_)));
    match (facing, side) {
        (Dir::W, Ins::L) | (Dir::E, Ins::R) => Dir::S,
        (Dir::W, Ins::R) | (Dir::E, Ins::L) => Dir::N,
        (Dir::N, Ins::L) | (Dir::S, Ins::R) => Dir::W,
        (Dir::N, Ins::R) | (Dir::S, Ins::L) => Dir::E,
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(y: usize, x: usize, dir: Dir) -> P {
        P { pos: (y, x), dir }
    }

    #[test]
    fn test_cube_pos() {
        // A1
        assert_eq!(next_pos_cube(&p(0, 50, Dir::W)), p(149, 0, Dir::E));
        assert_eq!(next_pos_cube(&p(49, 50, Dir::W)), p(100, 0, Dir::E));
        // A2
        assert_eq!(next_pos_cube(&p(100, 0, Dir::W)), p(49, 50, Dir::E));
        assert_eq!(next_pos_cube(&p(149, 0, Dir::W)), p(0, 50, Dir::E));
        // B1
        assert_eq!(next_pos_cube(&p(50, 50, Dir::W)), p(100, 0, Dir::S));
        assert_eq!(next_pos_cube(&p(99, 50, Dir::W)), p(100, 49, Dir::S));
        // B2
        assert_eq!(next_pos_cube(&p(100, 0, Dir::N)), p(50, 50, Dir::E));
        assert_eq!(next_pos_cube(&p(100, 49, Dir::N)), p(99, 50, Dir::E));
        // C1
        assert_eq!(next_pos_cube(&p(49, 100, Dir::S)), p(50, 99, Dir::W));
        assert_eq!(next_pos_cube(&p(49, 149, Dir::S)), p(99, 99, Dir::W));
        // C2
        assert_eq!(next_pos_cube(&p(50, 99, Dir::E)), p(49, 100, Dir::N));
        assert_eq!(next_pos_cube(&p(99, 99, Dir::E)), p(49, 149, Dir::N));
        // D1
        assert_eq!(next_pos_cube(&p(100, 99, Dir::E)), p(49, 149, Dir::W));
        assert_eq!(next_pos_cube(&p(149, 99, Dir::E)), p(0, 149, Dir::W));
        // D2
        assert_eq!(next_pos_cube(&p(0, 149, Dir::E)), p(149, 99, Dir::W));
        assert_eq!(next_pos_cube(&p(49, 149, Dir::E)), p(100, 99, Dir::W));
        // E1
        assert_eq!(next_pos_cube(&p(149, 50, Dir::S)), p(150, 49, Dir::W));
        assert_eq!(next_pos_cube(&p(149, 99, Dir::S)), p(199, 49, Dir::W));
        // E2
        assert_eq!(next_pos_cube(&p(150, 49, Dir::E)), p(149, 50, Dir::N));
        assert_eq!(next_pos_cube(&p(199, 49, Dir::E)), p(149, 99, Dir::N));
        // F1
        assert_eq!(next_pos_cube(&p(0, 100, Dir::N)), p(199, 0, Dir::N));
        assert_eq!(next_pos_cube(&p(0, 149, Dir::N)), p(199, 49, Dir::N));
        // F2
        assert_eq!(next_pos_cube(&p(199, 0, Dir::S)), p(0, 100, Dir::S));
        assert_eq!(next_pos_cube(&p(199, 49, Dir::S)), p(0, 149, Dir::S));
        // G1
        assert_eq!(next_pos_cube(&p(150, 0, Dir::W)), p(0, 50, Dir::S));
        assert_eq!(next_pos_cube(&p(199, 0, Dir::W)), p(0, 99, Dir::S));
        // G2
        assert_eq!(next_pos_cube(&p(0, 50, Dir::N)), p(150, 0, Dir::E));
        assert_eq!(next_pos_cube(&p(0, 99, Dir::N)), p(199, 0, Dir::E));
    }
}
