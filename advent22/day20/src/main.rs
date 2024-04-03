use std::io::prelude::*;

const KEY: isize = 811589153;

struct N {
    n: isize,
    ix: usize,
}

fn main() {
    let mut numbers: Vec<N> = std::io::stdin()
        .lock()
        .lines()
        .enumerate()
        .map(|(ix, n)| N {
            ix,
            n: KEY * n.ok().and_then(|s| s.parse::<isize>().ok()).unwrap(),
        })
        .collect();

    let len = numbers.len();

    for _ in 0..10 {
        for i in 0..len {
            // find N with original ix == i
            let ix = numbers.iter().position(|N { ix, .. }| *ix == i).unwrap();
            // get new location based on N.n
            let new_ix = match numbers[ix].n {
                0 => {
                    continue;
                }
                n => ((ix as isize + n).rem_euclid(len as isize - 1)) as usize,
            };
            // move item
            let n = numbers.remove(ix);
            numbers.insert(new_ix, n);
        }
    }

    let zero = numbers.iter().position(|N { n, .. }| *n == 0).unwrap();
    let n1 = numbers[(zero + 1000) % len].n;
    let n2 = numbers[(zero + 2000) % len].n;
    let n3 = numbers[(zero + 3000) % len].n;
    dbg!(n1, n2, n3, n1 + n2 + n3);
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_mod() {
        // [0, 1, 2, 3]
        // pos ^
        let len = 4;
        let n = -3;
        let pos = 1isize;
        let target = (pos + n).rem_euclid(len);
        dbg!(target);
    }
}
