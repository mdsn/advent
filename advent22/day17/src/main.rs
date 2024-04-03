#![feature(variant_count)]

use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::io;
use std::io::prelude::*;
use std::mem;
use std::ops::{Add, AddAssign};

#[derive(Debug, Clone, Copy, PartialEq)]
enum Dir {
    L,
    R,
    D,
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct P {
    x: usize,
    y: usize,
}

impl Add<Dir> for P {
    type Output = Self;
    fn add(self, rhs: Dir) -> Self {
        let v = match rhs {
            Dir::L => (-1, 0),
            Dir::R => (1, 0),
            Dir::D => (0, -1),
        };
        Self {
            x: (self.x as isize + v.0) as usize,
            y: (self.y as isize + v.1) as usize,
        }
    }
}

impl AddAssign<Dir> for P {
    fn add_assign(&mut self, rhs: Dir) {
        let v = match rhs {
            Dir::L => (-1, 0),
            Dir::R => (1, 0),
            Dir::D => (0, -1),
        };
        self.x = (self.x as isize + v.0) as usize;
        self.y = (self.y as isize + v.1) as usize;
    }
}

impl From<(usize, usize)> for P {
    fn from(value: (usize, usize)) -> Self {
        P {
            x: value.0,
            y: value.1,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Polyomino {
    Dash,
    Plus,
    L,
    Pipe,
    Square,
}

impl Polyomino {
    fn height(&self) -> usize {
        match *self {
            Polyomino::Dash => 1,
            Polyomino::Plus => 3,
            Polyomino::L => 3,
            Polyomino::Pipe => 4,
            Polyomino::Square => 2,
        }
    }

    fn width(&self) -> usize {
        match *self {
            Polyomino::Dash => 4,
            Polyomino::Plus => 3,
            Polyomino::L => 3,
            Polyomino::Pipe => 1,
            Polyomino::Square => 2,
        }
    }
}

fn polyominos() -> PolyManufacture {
    PolyManufacture { i: 0 }
}

struct PolyManufacture {
    i: usize,
}

impl Iterator for PolyManufacture {
    type Item = Polyomino;
    fn next(&mut self) -> Option<Self::Item> {
        let poly = match self.i {
            0 => Polyomino::Dash,
            1 => Polyomino::Plus,
            2 => Polyomino::L,
            3 => Polyomino::Pipe,
            4 => Polyomino::Square,
            _ => unreachable!(),
        };
        self.i = (self.i + 1) % mem::variant_count::<Polyomino>();
        Some(poly)
    }
}

#[derive(Debug, Clone)]
struct Rock {
    poly: Polyomino,
    pos: P, // position of bottom-left tile in the chamber
}

impl Rock {
    fn valid_move(&self, dir: Dir) -> bool {
        match dir {
            Dir::L => self.pos.x > 0,
            Dir::R => self.pos.x + self.poly.width() < CHAMBER_WIDTH,
            Dir::D => self.pos.y > 0, // XXX consider highest rock!
        }
    }

    fn tiles(&self) -> Vec<P> {
        let P { x, y } = self.pos;
        match self.poly {
            Polyomino::Dash => vec![
                (x, y).into(),
                (x + 1, y).into(),
                (x + 2, y).into(),
                (x + 3, y).into(),
            ],
            // .#.
            // ###
            // .#.
            Polyomino::Plus => vec![
                (x + 1, y).into(),
                (x, y + 1).into(),
                (x + 1, y + 1).into(),
                (x + 2, y + 1).into(),
                (x + 1, y + 2).into(),
            ],
            // ..#
            // ..#
            // ###
            Polyomino::L => vec![
                (x, y).into(),
                (x + 1, y).into(),
                (x + 2, y).into(),
                (x + 2, y + 1).into(),
                (x + 2, y + 2).into(),
            ],
            Polyomino::Pipe => vec![
                (x, y).into(),
                (x, y + 1).into(),
                (x, y + 2).into(),
                (x, y + 3).into(),
            ],
            Polyomino::Square => vec![
                (x, y).into(),
                (x + 1, y).into(),
                (x, y + 1).into(),
                (x + 1, y + 1).into(),
            ],
        }
    }
}

const CHAMBER_WIDTH: usize = 7;

#[derive(Debug)]
struct Chamber {
    buf: Vec<[u8; CHAMBER_WIDTH]>,
    rock: Option<Rock>,
    rock_height: usize,
}

impl Chamber {
    fn new() -> Self {
        Self {
            buf: vec![[0; CHAMBER_WIDTH]; 3],
            rock: None,
            rock_height: 0,
        }
    }

    fn height(&self) -> usize {
        self.buf.len()
    }

    #[allow(dead_code)]
    fn print(&self) {
        let rock_tiles = if let Some(rock) = &self.rock {
            rock.tiles()
        } else {
            vec![]
        };
        for (y, row) in self.buf.iter().enumerate().rev() {
            self.print_row(row, y, &rock_tiles);
        }
        println!("+-------+");
    }

    #[allow(dead_code)]
    #[inline]
    fn print_row(&self, row: &[u8; CHAMBER_WIDTH], y: usize, rock_tiles: &[P]) {
        print!("|");
        for (x, tile) in row.iter().enumerate() {
            print!(
                "{}",
                if rock_tiles.contains(&P { x, y }) {
                    '@'
                } else if *tile == 0 {
                    '.'
                } else {
                    '#'
                }
            );
        }
        if y == self.height() - 1 {
            println!("| {}", self.height());
        } else if self.rock_height > 0 && y == self.rock_height - 1 {
            println!("| {}", self.rock_height);
        } else {
            println!("|");
        }
    }

    fn materialize_rock(&mut self, poly: Polyomino) {
        assert!(self.rock.is_none());

        let empty_rows = self.height() - self.rock_height;
        let poly_height = poly.height();

        if empty_rows < poly_height + 3 {
            for _ in 0..(poly_height + 3) - empty_rows {
                self.buf.push([0; CHAMBER_WIDTH]);
            }
        }

        let spawn_y = (self.height() - poly_height).min(self.rock_height + 3);
        let rock = Rock {
            poly,
            pos: P { x: 2, y: spawn_y },
        };

        self.rock = Some(rock);
    }

    fn ossify_rock(&mut self) -> Rock {
        assert!(self.rock.is_some());
        let rock = self.rock.take().unwrap();
        let tiles = rock.tiles();
        self.emplace_tiles(&tiles, 1);
        self.rock_height = self
            .rock_height
            .max(1 + tiles.iter().map(|t| t.y).max().unwrap());
        rock
    }

    fn emplace_tiles(&mut self, tiles: &[P], value: u8) {
        for t in tiles {
            self.buf[t.y][t.x] = value;
        }
    }

    fn check_for_collision(&self, tiles: &[P], dir: Dir) -> bool {
        tiles
            .iter()
            .map(|t| *t + dir)
            .any(|t| self.buf[t.y][t.x] == 1)
    }

    fn move_rock(&mut self, dir: Dir) {
        assert!(self.rock.is_some());
        let mut rock = self.rock.clone().unwrap();
        let tiles = rock.tiles();
        if rock.valid_move(dir) && !self.check_for_collision(&tiles, dir) {
            rock.pos += dir;
        }
        self.rock = Some(rock);
    }

    fn rock_at_rest(&self) -> bool {
        assert!(self.rock.is_some());
        let rock = self.rock.as_ref().unwrap();
        if rock.valid_move(Dir::D) {
            let tiles = rock.tiles();
            self.check_for_collision(&tiles, Dir::D)
        } else {
            true
        }
    }

    // return an array of the highest rocks in each column relative to self.rock_height
    fn top_formation(&self) -> [i32; CHAMBER_WIDTH] {
        let mut res = [i32::MIN; CHAMBER_WIDTH];
        let mut y = self.rock_height;
        loop {
            if res.iter().all(|v| *v != i32::MIN) {
                break;
            }
            let row = &self.buf[y];
            for x in 0..CHAMBER_WIDTH {
                if row[x] == 1 {
                    res[x] = res[x].max(y as i32);
                }
            }
            if y > 0 {
                y -= 1;
            } else {
                for v in res.iter_mut() {
                    if *v == i32::MIN {
                        *v = 0;
                    }
                }
                break;
            }
        }
        let max = *res.iter().max().unwrap();
        for v in res.iter_mut() {
            *v -= max;
        }
        res
    }
}

fn main() {
    let jets: Vec<Dir> = io::stdin()
        .lock()
        .lines()
        .map(Result::unwrap)
        .take(1)
        .flat_map(|s| {
            s.chars()
                .map(|c| match c {
                    '<' => Dir::L,
                    '>' => Dir::R,
                    _ => unreachable!(),
                })
                .collect::<Vec<Dir>>()
        })
        .collect();

    let mut chamber = Chamber::new();
    let mut jet_idx = 0;

    // when a piece lands, if the tops of the columns form an already seen formation
    // for an already seen jet index and an already seen piece, we have found a cycle.
    let mut seen = HashMap::new();

    let mut polys = polyominos();
    let mut i: usize = 0;
    let mut added_height = 0;
    while i < 1_000_000_000_000 {
        let poly = polys.next().unwrap();
        chamber.materialize_rock(poly);

        loop {
            let jet = jets[jet_idx];
            chamber.move_rock(jet);

            if chamber.rock_at_rest() {
                i += 1;

                let rock = chamber.ossify_rock();
                let key = (chamber.top_formation(), rock.poly as u8, jet_idx);
                jet_idx = (jet_idx + 1) % jets.len();

                if let Entry::Vacant(e) = seen.entry(key) {
                    e.insert((i, chamber.rock_height));
                } else {
                    let (old_i, old_height) = seen.remove(&key).unwrap();
                    let height_dif = chamber.rock_height - old_height;
                    let cycle_len = i - old_i;
                    let cycles = (1_000_000_000_000 - i) / cycle_len;
                    added_height += height_dif * cycles;
                    i += cycle_len * cycles;
                }

                break;
            } else {
                chamber.move_rock(Dir::D);
            }

            jet_idx = (jet_idx + 1) % jets.len();
        }
    }

    dbg!(chamber.rock_height + added_height);
}
