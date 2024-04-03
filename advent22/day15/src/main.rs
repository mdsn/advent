#![feature(slice_take)]

use std::io;
use std::io::prelude::*;

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone)]
struct Range {
    lo: i32,
    hi: i32,
}

impl Range {
    fn new(lo: i32, hi: i32) -> Self {
        assert!(lo <= hi);
        Range { lo, hi }
    }

    fn extend(&mut self, other: &Range) {
        assert!(self.overlaps(other));
        self.lo = self.lo.min(other.lo);
        self.hi = self.hi.max(other.hi);
    }

    fn overlaps(&self, r: &Range) -> bool {
        !(self.hi < r.lo || self.lo > r.hi) || (self.hi - r.lo).abs() == 1
    }
}

#[derive(Debug)]
struct Set {
    inner: Vec<Range>,
}

impl Set {
    fn new() -> Self {
        Set { inner: vec![] }
    }

    fn disjoint(&self) -> bool {
        self.inner.len() > 1
    }

    fn hole(&self) -> i32 {
        assert_eq!(self.inner.len(), 2);
        (self.inner[0].hi + self.inner[1].lo) / 2
    }

    fn union(&mut self, lo: i32, hi: i32) {
        self.inner.push(Range::new(lo, hi));
        self.inner.sort();
        let (head, mut tail) = self.inner.split_at_mut(1);
        let mut merged = vec![head[0].clone()];
        while !tail.is_empty() {
            let mut head = merged.pop().unwrap();
            let r = tail.take_first_mut().unwrap();
            if head.overlaps(r) {
                head.extend(r);
                merged.push(head);
            } else {
                merged.push(head);
                merged.push(r.clone());
            }
        }
        self.inner = merged;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_range_overlaps() {
        assert!(Range::new(-1, 1).overlaps(&Range::new(0, 2)));
        assert!(Range::new(-1, 1).overlaps(&Range::new(1, 2)));
        assert!(Range::new(0, 1).overlaps(&Range::new(2, 3))); // [0,1]U[2,3] = [0,3]
    }

    #[test]
    fn test_range_extend() {
        let mut r1 = Range::new(0, 2);
        r1.extend(&Range::new(2, 4));
        assert_eq!((r1.lo, r1.hi), (0, 4));
        let mut r1 = Range::new(0, 2);
        r1.extend(&Range::new(1, 5));
        assert_eq!((r1.lo, r1.hi), (0, 5));
    }

    #[test]
    fn test_set_union() {
        let mut set = Set::new();
        set.union(3, 4);
        assert_eq!(set.inner[0], Range::new(3, 4));
        set.union(1, 2);
        assert_eq!(set.inner.len(), 1); // [1,2]U[3,4] = [1,4]
        assert_eq!(set.inner, vec![Range::new(1, 4)]);
        set.union(2, 3);
        assert_eq!(set.inner.len(), 1);
        set.union(2, 5);
        assert_eq!(set.inner.len(), 1);
        assert_eq!(set.inner[0], Range::new(1, 5));
        set.union(-1, 0); // [-1,0] U [1,5] = [-1, 5]
        assert_eq!(set.inner.len(), 1);
        assert_eq!(set.inner, vec![Range::new(-1, 5)]);
    }
}

type P = (i32, i32);

#[derive(Debug)]
struct Sensor {
    pos: P,
    beacon: P,
}

fn distance(a: P, b: P) -> i32 {
    (a.0 - b.0).abs() + (a.1 - b.1).abs()
}

fn main() {
    let handle = io::stdin().lock();
    let mut sensors = vec![];

    for line in handle.lines().map(Result::unwrap) {
        let coords: Vec<_> = line
            .split_terminator(&[',', ':'])
            .map(|s| s.split('=').last().unwrap().parse::<i32>().unwrap())
            .collect();
        sensors.push(Sensor {
            pos: (coords[0], coords[1]),
            beacon: (coords[2], coords[3]),
        });
    }

    let k = 4000000;
    for y in 0..=k {
        let mut set = Set::new();

        for sensor in &sensors {
            let range = distance(sensor.pos, sensor.beacon);
            let dist_row = (sensor.pos.1 - y).abs();
            if dist_row <= range {
                let dx = range - dist_row;
                let (lo, hi) = (sensor.pos.0 - dx, sensor.pos.0 + dx);
                set.union(lo, hi);
            }
        }

        if set.disjoint() {
            let x = set.hole() as u64;
            let part2: u64 = x * 4_000_000 + y as u64;
            dbg!(part2);
            break;
        }
    }
}
