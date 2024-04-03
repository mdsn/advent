use std::io;
use std::io::prelude::*;

#[derive(Debug)]
struct Cave {
    buf: Vec<Vec<char>>,
    x_range: (usize, usize),
    y_range: (usize, usize),
}

type Point = (usize, usize);

impl Cave {
    fn translate(&self, p: Point) -> Point {
        (p.0 - self.x_range.0, p.1)
    }

    fn dimensions(&self) -> Point {
        (self.buf[0].len(), self.buf.len())
    }

    fn at_left_border(&self, sand: Point) -> bool {
        self.translate(sand).0 == 0
    }

    fn beyond_right_border(&self, sand: Point) -> bool {
        self.translate(sand).0 == self.dimensions().0
    }

    fn expand_left(&mut self) {
        self.x_range.0 -= 1;
        for row in self.buf.iter_mut() {
            row.insert(0, '.');
        }
        let (w, h) = self.dimensions();
        self.buf[h - 1][w - 1] = '#';
    }

    fn expand_right(&mut self) {
        self.x_range.1 += 1;
        for row in self.buf.iter_mut() {
            row.push('.');
        }
        let (w, h) = self.dimensions();
        self.buf[h - 1][w - 1] = '#';
    }

    fn place_tile(&mut self, tile: char, p: Point) {
        let (x, y) = self.translate(p);
        self.buf[y][x] = tile;
    }

    fn get_tile(&self, p: Point) -> Option<char> {
        if p.1 == self.y_range.1 {
            return Some('#');
        }
        if p.0 < self.x_range.0 || p.0 > self.x_range.1 || p.1 > self.y_range.1 {
            return None;
        }
        let (x, y) = self.translate(p);
        Some(self.buf[y][x])
    }
}

fn can_move_down(cave: &Cave, sand: Point) -> bool {
    if let Some(tile) = cave.get_tile((sand.0, sand.1 + 1)) {
        tile == '.'
    } else {
        unreachable!(); // part 2 - unlimited floor
    }
}

fn can_move_down_left(cave: &Cave, sand: Point) -> bool {
    if let Some(tile) = cave.get_tile((sand.0 - 1, sand.1 + 1)) {
        tile == '.'
    } else {
        true
    }
}

fn can_move_down_right(cave: &Cave, sand: Point) -> bool {
    if let Some(tile) = cave.get_tile((sand.0 + 1, sand.1 + 1)) {
        tile == '.'
    } else {
        true
    }
}

fn main() {
    let handle = io::stdin().lock();
    let mut formations = vec![];

    let mut x_range = (usize::MAX, 0usize);
    let mut y_range = (usize::MAX, 0usize);

    for line in handle.lines() {
        let line = line.unwrap();
        let points: Vec<Point> = line
            .split(" -> ")
            .map(|p| {
                let p: Vec<_> = p.split(',').map(|q| q.parse::<usize>().unwrap()).collect();

                x_range.0 = x_range.0.min(p[0]);
                x_range.1 = x_range.1.max(p[0]);
                y_range.0 = y_range.0.min(p[1]);
                y_range.1 = y_range.1.max(p[1]);

                (p[0], p[1])
            })
            .collect();
        formations.push(points);
    }

    y_range.1 += 2;
    let mut cave = Cave {
        buf: vec![vec!['.'; x_range.1 - x_range.0 + 1]; y_range.1 + 1],
        x_range,
        y_range,
    };

    for formation in &formations {
        formation.as_slice().windows(2).for_each(|w| {
            let mut p = [w[0], w[1]];
            if p[0].0 == p[1].0 {
                p.sort_by_key(|q| q.1);
                for y in p[0].1..=p[1].1 {
                    cave.place_tile('#', (p[0].0, y));
                }
            } else {
                p.sort_by_key(|q| q.0);
                for x in p[0].0..=p[1].0 {
                    cave.place_tile('#', (x, p[0].1));
                }
            }
        });
    }

    let mut sand_at_rest = 0;
    loop {
        let mut sand = (500, 0);
        let mut at_rest = false;
        while !at_rest {
            if cave.at_left_border(sand) && can_move_down_left(&cave, sand) {
                cave.expand_left();
            } else if cave.beyond_right_border(sand) {
                cave.expand_right();
            }

            if can_move_down(&cave, sand) {
                sand = (sand.0, sand.1 + 1);
            } else if can_move_down_left(&cave, sand) {
                sand = (sand.0 - 1, sand.1 + 1);
            } else if can_move_down_right(&cave, sand) {
                sand = (sand.0 + 1, sand.1 + 1);
            } else {
                cave.place_tile('o', sand);
                sand_at_rest += 1;
                if sand == (500, 0) {
                    dbg!(sand_at_rest);
                    return;
                }
                at_rest = true;
            }
        }
    }
}
