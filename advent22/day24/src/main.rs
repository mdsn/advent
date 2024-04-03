use std::collections::HashSet;
use std::io::prelude::*;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
enum Dir {
    U,
    D,
    L,
    R,
}

#[derive(PartialEq, Eq, Hash, Debug)]
struct Blizzard {
    pos: (i32, i32),
    dir: Dir,
}

#[derive(Debug)]
struct TimeSlice {
    blizzards: HashSet<Blizzard>,
}

impl TimeSlice {
    fn blizzard_at(&self, x: i32, y: i32) -> Option<&Blizzard> {
        self.blizzards
            .iter()
            .find(|Blizzard { pos: (x0, y0), .. }| x == *x0 && y == *y0)
    }
}

#[derive(Debug)]
struct Time {
    width: i32,
    height: i32,
    slices: Vec<TimeSlice>,
}

impl Time {
    fn tick(&mut self) {
        let blizzards = self
            .slices
            .last()
            .unwrap()
            .blizzards
            .iter()
            .map(|b| {
                let (x0, y0) = b.pos;
                Blizzard {
                    pos: match b.dir {
                        Dir::U => (x0, if y0 == 0 { self.height - 1 } else { y0 - 1 }),
                        Dir::D => (x0, (y0 + 1) % self.height),
                        Dir::L => (if x0 == 0 { self.width - 1 } else { x0 - 1 }, y0),
                        Dir::R => ((x0 + 1) % self.width, y0),
                    },
                    dir: b.dir,
                }
            })
            .collect::<HashSet<_>>();
        self.slices.push(TimeSlice { blizzards });
    }

    #[allow(dead_code)]
    fn print_t(&self, t: usize) {
        assert!(t < self.slices.len());
        let slice = &self.slices[t];
        // header
        print!("#.");
        for _ in 0..self.width {
            print!("#");
        }
        println!();
        // rows
        for y in 0..self.height {
            print!("#");
            for x in 0..self.width {
                if let Some(Blizzard { dir, .. }) = slice.blizzard_at(x, y) {
                    print!(
                        "{}",
                        match dir {
                            Dir::U => '^',
                            Dir::D => 'v',
                            Dir::L => '<',
                            Dir::R => '>',
                        }
                    );
                } else {
                    print!(".");
                }
            }
            println!("#");
        }
        // footer
        for _ in 0..self.width {
            print!("#");
        }
        println!(".#");
    }

    #[allow(dead_code)]
    fn print_last(&self) {
        self.print_t(self.slices.len() - 1);
    }
}

fn main() {
    let mut lines = std::io::stdin()
        .lock()
        .lines()
        .skip(1)
        .map(Result::unwrap)
        .collect::<Vec<_>>();
    lines.pop();

    let width = (lines[0].len() - 2) as i32;
    let height = lines.len() as i32;

    let mut blizzards = HashSet::new();
    for (y, row) in lines.iter().enumerate() {
        for (x, c) in row.chars().skip(1).enumerate() {
            let bliz = Blizzard {
                pos: (x as i32, y as i32),
                dir: match c {
                    '>' => Dir::R,
                    '^' => Dir::U,
                    '<' => Dir::L,
                    'v' => Dir::D,
                    _ => continue,
                },
            };
            blizzards.insert(bliz);
        }
    }

    let mut time = Time {
        width,
        height,
        slices: vec![TimeSlice { blizzards }],
    };

    let mut seen = HashSet::new();

    let mut do_it = |start: (i32, i32), target: (i32, i32), t: usize| -> usize {
        seen.clear();
        dfs(&mut time, start, start, target, t, &mut seen)
    };

    let part1 = do_it((0, -1), (width - 1, height - 1), 0);
    dbg!(part1);
    let time_back = do_it((width - 1, height), (0, 0), part1);
    let part2 = do_it((0, -1), (width - 1, height - 1), time_back);
    dbg!(part2);
}

fn dfs(
    time: &mut Time,
    start: (i32, i32),
    pos: (i32, i32),
    target: (i32, i32),
    t: usize,
    seen: &mut HashSet<(i32, i32, usize)>,
) -> usize {
    let (x, y) = pos;
    assert!(time.slices[t].blizzard_at(x, y).is_none());
    seen.insert((x, y, t));

    // hard stop at t = 2000 not to blow the stack
    if t == 2000 {
        return usize::MAX;
    }

    if pos == target {
        return t + 1;
    }

    if t == time.slices.len() - 1 {
        time.tick();
    }

    // dfs on each available neighbor at t + 1
    let next_slice = &time.slices[t + 1];
    let neighbors = if pos == start {
        let (x0, y0) = match start {
            (0, -1) => (0, 0),
            _ => (time.width - 1, time.height - 1),
        };
        match next_slice.blizzard_at(x0, y0) {
            None => vec![(x0, y0), start],
            Some(_) => vec![start],
        }
    } else {
        [(-1, 0), (0, -1), (0, 1), (1, 0), (0, 0)]
            .into_iter()
            .filter_map(|(dx, dy)| {
                let (x0, y0) = (x + dx, y + dy);
                if x0 < 0
                    || x0 >= time.width
                    || y0 < 0
                    || y0 >= time.height
                    || seen.contains(&(x0, y0, t + 1))
                    || next_slice.blizzard_at(x0, y0).is_some()
                {
                    None
                } else {
                    Some((x0, y0))
                }
            })
            .collect()
    };

    neighbors
        .into_iter()
        .map(|(x0, y0)| dfs(time, start, (x0, y0), target, t + 1, seen))
        .min()
        .unwrap_or(usize::MAX)
}
