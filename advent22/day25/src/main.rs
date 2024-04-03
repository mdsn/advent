use itertools::EitherOrBoth::{Both, Left, Right};
use itertools::Itertools;
use std::io::prelude::*;

fn digitize(s: &str) -> Vec<i32> {
    s.chars()
        .rev()
        .map(|c| match c {
            '=' => -2,
            '-' => -1,
            d => d.to_digit(10).unwrap().try_into().unwrap(),
        })
        .collect()
}

fn snafu(ds: &[i32]) -> String {
    ds.iter()
        .map(|d| match d {
            -2 => '=',
            -1 => '-',
            0 => '0',
            1 => '1',
            2 => '2',
            _ => unreachable!(),
        })
        .collect()
}

fn add_snafu(r: &str, s: &str) -> String {
    let mut carry = 0;
    let mut result = vec![];
    for it in digitize(r).iter().zip_longest(digitize(s).iter()) {
        let sum = match it {
            Both(a, b) => a + b + carry,
            Left(a) | Right(a) => a + carry,
        };
        let (q, r) = (sum.div_euclid(5), sum.rem_euclid(5));
        let (q, r) = if r > 2 { (q + 1, r - 5) } else { (q, r) };
        result.push(r);
        carry = q;
    }
    result[..].reverse();
    snafu(&result)
}

fn main() {
    let result = std::io::stdin()
        .lock()
        .lines()
        .map(Result::unwrap)
        .reduce(|acc, e| add_snafu(&acc, &e))
        .unwrap();
    dbg!(result);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn snafu_to_i32(value: &str) -> i32 {
        value
            .chars()
            .rev()
            .enumerate()
            .map(|(i, d)| {
                num::pow(5, i)
                    * match d {
                        '=' => -2,
                        '-' => -1,
                        c => c.to_digit(10).unwrap().try_into().unwrap(),
                    }
            })
            .sum()
    }

    #[test]
    fn test_snafu() {
        let s = "2=-01";
        let mut n = digitize(s);
        n[..].reverse();
        let t = snafu(&n);
        assert_eq!(s, t);
    }

    #[test]
    fn test_divrem() {
        let t = add_snafu("1=-0-2", "2=-01");
        assert_eq!(snafu_to_i32(&t), 976 + 1747);
    }
}
