use std::collections::{HashSet, VecDeque};
use std::io;
use std::io::prelude::*;

type Pos = [u8; 3];

fn adjacent_positions(pos: Pos) -> Vec<Pos> {
    let mut neighbors = vec![];
    let faces = [
        [1i8, 0, 0],
        [0, 1, 0],
        [0, 0, 1],
        [-1, 0, 0],
        [0, -1, 0],
        [0, 0, -1],
    ];
    let [x, y, z] = pos;
    for [x0, y0, z0] in faces
        .iter()
        .map(|[dx, dy, dz]| [x as i8 - dx, y as i8 - dy, z as i8 - dz])
        .filter(|[x0, y0, z0]| {
            *x0 >= 0 && *y0 >= 0 && *z0 >= 0 && *x0 <= 20 && *y0 <= 20 && *z0 <= 20
        })
    {
        neighbors.push([x0 as u8, y0 as u8, z0 as u8]);
    }
    neighbors
}

fn neighbors(grid: &HashSet<Pos>, pos: Pos) -> Vec<Pos> {
    let mut neighbors = vec![];
    for neighbor in adjacent_positions(pos)
        .into_iter()
        .filter(|[x0, y0, z0]| grid.contains(&[*x0, *y0, *z0]))
    {
        neighbors.push(neighbor);
    }

    neighbors
}

fn main() {
    let mut grid = HashSet::new();

    for line in io::stdin().lock().lines().map(Result::unwrap) {
        let cube: Pos = line
            .split(',')
            .map(|s| s.parse().unwrap())
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();
        grid.insert(cube);
    }

    let total: usize = grid.iter().map(|qb| 6 - neighbors(&grid, *qb).len()).sum();
    dbg!(total);

    // 3D flood fill
    let adjacent: HashSet<Pos> = HashSet::from_iter(grid.iter().flat_map(|q| {
        adjacent_positions(*q)
            .into_iter()
            .filter(|adj| !grid.contains(adj))
    }));
    let mut trapped = HashSet::new();
    let mut all_visited = HashSet::new();
    let mut queue = VecDeque::new();

    loop {
        let mut outside = false;
        let mut visited = HashSet::new();

        // if adjacent - all_visited is empty, there are no more adjacents to check, break.
        if adjacent.difference(&all_visited).next().is_none() {
            break;
        }

        // take arbitrary cube from adjacent that is not `all_visited` and queue it
        let cube = adjacent.iter().find(|q| !all_visited.contains(*q)).unwrap();
        queue.push_back(*cube);
        while !queue.is_empty() {
            let qb = queue.pop_front().unwrap();

            visited.insert(qb);
            all_visited.insert(qb);

            // if it touches the boundaries at 0 or 20
            if qb.iter().any(|xyz| *xyz == 0 || *xyz >= 20) {
                outside = true;
            }

            // take every non-visited+non-all_visited neighbor that is not in the grid, queue them
            let ns = adjacent_positions(qb);
            for n in ns {
                if !queue.contains(&n) // why is this even necessary :S
                    && !visited.contains(&n)
                    && !all_visited.contains(&n)
                    && !grid.contains(&n)
                {
                    queue.push_back(n);
                }
            }
        }

        if !outside {
            visited.drain().for_each(|q| {
                trapped.insert(q);
            });
        }
    }

    // count faces of `trapped` that touch cubes in `grid`, subtract from `total`, report. The end
    let closed: usize = trapped.iter().map(|qb| neighbors(&grid, *qb).len()).sum();
    dbg!(total - closed);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_neighbors() {
        let grid = HashSet::from([[0, 0, 0], [1, 0, 0]]);
        let p0 = [0, 0, 0];
        let ns = neighbors(&grid, p0);
        dbg!(ns);
    }
}
