#![feature(array_windows)]

use itertools::Itertools;
use std::collections::HashMap;
use std::io;

#[derive(Debug, Clone)]
struct Neighbor {
    name: String,
    distance: u32,
}

#[derive(Debug, Clone)]
struct Valve {
    name: String,
    flow: u32,
    neighbors: Vec<Neighbor>,
}

const AVAILABLE_TIME: u32 = 26;

fn main() {
    let mut nz_valves = vec![];
    let mut valves = vec![];
    for line in io::stdin().lines().map(Result::unwrap) {
        let parts: Vec<_> = line.split(' ').collect();
        let name = parts[1].to_string();
        let flow = parts[4]
            .split(&['=', ';'])
            .nth(1)
            .unwrap()
            .parse::<u32>()
            .unwrap();
        let neighbors: Vec<_> = parts[9..]
            .iter()
            .map(|s| Neighbor {
                name: s.trim_end_matches(',').to_string(),
                distance: 1,
            })
            .collect();

        if name == "AA" {
            nz_valves.insert(
                0,
                Valve {
                    name: "AA".to_string(),
                    flow,
                    neighbors: vec![],
                },
            );
        }

        if flow > 0 {
            // all >0 valves are neighbors of one another in the condensed graph
            nz_valves.push(Valve {
                name: name.clone(),
                flow,
                neighbors: vec![],
            });
        }

        valves.push(Valve {
            name,
            flow,
            neighbors,
        });
    }

    // indices of non-zero valves in the entire vector of valves
    let mut nz_indices: HashMap<&String, usize> = [].into();
    // indices of all valves
    let indices: HashMap<&String, usize> = valves
        .iter()
        .enumerate()
        .map(|(i, v)| {
            if v.flow > 0 {
                nz_indices.insert(&v.name, i);
            }
            (&v.name, i)
        })
        .collect();

    // indices of non-zero valves in the derived vector of non-zero valves
    let useful_indices: HashMap<String, usize> = nz_valves
        .iter()
        .enumerate()
        .map(|(i, v)| (v.name.to_string(), i))
        .collect();

    let dist = floyd_warshall(&valves, &indices);
    let mut nz_dist = vec![vec![0u32; nz_valves.len()]; nz_valves.len()];

    for valve in &valves {
        if valve.name != "AA" && valve.flow == 0 {
            continue;
        }
        let idx = indices[&valve.name]; // index into distance matrix
        let nzidx = useful_indices[&valve.name]; // index of valve in reduced graph
        let nzvalve = &mut nz_valves[nzidx]; // valve copy in the derived
                                             // graph
        for (name, neighbor_idx) in &nz_indices {
            if **name == valve.name {
                continue;
            }
            let distance = dist[idx][*neighbor_idx];

            let neighbor_nzidx = useful_indices[*name];
            nz_dist[nzidx][neighbor_nzidx] = distance;
            nz_dist[neighbor_nzidx][nzidx] = distance;

            nzvalve.neighbors.push(Neighbor {
                name: name.to_string(),
                distance,
            });
        }
    }

    let mut presh = 0u32;
    let mut combinations = nz_valves[1..]
        .iter()
        .map(|v| v.name.to_string())
        .combinations(7)
        .collect::<Vec<_>>();
    for subset in combinations.iter_mut() {
        let mut other: Vec<_> = nz_valves[1..]
            .iter()
            .filter_map(|v| {
                if !subset.contains(&v.name) {
                    Some(v.name.to_string())
                } else {
                    None
                }
            })
            .collect();
        subset.insert(0, "AA".to_string());
        other.insert(0, "AA".to_string());

        let released1 = dfs(
            &mut vec!["AA".to_string()],
            AVAILABLE_TIME,
            0,
            &useful_indices,
            &nz_valves,
            &nz_dist,
            subset,
        );
        let released2 = dfs(
            &mut vec!["AA".to_string()],
            AVAILABLE_TIME,
            0,
            &useful_indices,
            &nz_valves,
            &nz_dist,
            &other,
        );

        presh = presh.max(released1 + released2);
    }
    dbg!(presh);
}

fn dfs(
    path: &mut Vec<String>,
    time: u32,
    mut best_so_far: u32,
    indices: &HashMap<String, usize>,
    valves: &Vec<Valve>,
    distances: &[Vec<u32>],
    allowed: &[String],
) -> u32 {
    let name = &path[path.len() - 1];
    let valve = &valves[indices[name]];

    let not_visited: Vec<&Neighbor> = valve
        .neighbors
        .iter()
        .filter(|n| allowed.contains(&n.name) && !path.contains(&n.name))
        .collect();
    if not_visited.is_empty() {
        return total_released_pressure(path, distances, indices, valves);
    }

    for neighbor in not_visited {
        if neighbor.distance >= time {
            best_so_far =
                best_so_far.max(total_released_pressure(path, distances, indices, valves));
            continue;
        }
        path.push(neighbor.name.to_string());
        best_so_far = best_so_far.max(dfs(
            path,
            time - neighbor.distance - 1,
            best_so_far,
            indices,
            valves,
            distances,
            allowed,
        ));
        path.pop();
    }
    best_so_far
}

fn total_released_pressure(
    order: &[String],
    dist: &[Vec<u32>],
    indices: &HashMap<String, usize>,
    valves: &[Valve],
) -> u32 {
    let mut time = AVAILABLE_TIME;
    let mut total = 0;
    for [from, to] in order.array_windows::<2>() {
        let (i, j) = (indices[from], indices[to]);
        if time > dist[i][j] {
            time = time - dist[i][j] - 1;
            total += time * valves[j].flow;
        }
    }
    total
}

// sounds expensive!
fn floyd_warshall(g: &Vec<Valve>, indices: &HashMap<&String, usize>) -> Vec<Vec<u32>> {
    let mut adj = vec![vec![false; g.len()]; g.len()];
    for (i, v) in g.iter().enumerate() {
        for neighbor in &v.neighbors {
            let j = indices[&neighbor.name];
            adj[i][j] = true;
            adj[j][i] = true;
        }
    }

    let mut dist = vec![vec![u32::MAX; g.len()]; g.len()];
    for i in 0..g.len() {
        for j in 0..g.len() {
            if i == j {
                dist[i][j] = 0;
            } else if adj[i][j] {
                dist[i][j] = 1;
            }
        }
    }

    for k in 0..g.len() {
        for i in 0..g.len() {
            for j in 0..g.len() {
                if !&[dist[i][k], dist[k][j]].contains(&u32::MAX) {
                    dist[i][j] = dist[i][j].min(dist[i][k] + dist[k][j]);
                }
            }
        }
    }

    dist
}
