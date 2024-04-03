use rayon::prelude::*;
use std::io;
use std::io::prelude::*;

#[derive(Debug, Clone, Copy)]
struct Blueprint {
    ore: u32,             // ore
    clay: u32,            // ore
    obsidian: (u32, u32), // ore, clay
    geode: (u32, u32),    // ore, obsidian
}

#[derive(Debug, Default, Clone, Copy)]
struct Counters {
    ore: u32,
    clay: u32,
    obsidian: u32,
    geode: u32,
}

#[derive(Debug, Clone)]
struct Ctx {
    blueprint: Blueprint,
    robots: Counters,
    resources: Counters,
    time_left: u32,
}

const NEXT_BUILDS: [[u32; 4]; 4] = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]];

fn recurse(mut ctx: Ctx, next: [u32; 4]) -> u32 {
    if ctx.time_left == 0 {
        return ctx.resources.geode;
    }

    let mut built = false;
    if enough_resources(&ctx.resources, &ctx.blueprint, &next) {
        built = true;
        start_build(&mut ctx, &next);
    } else if !enough_time(&ctx, &next) {
        let last_geodes = ctx.robots.geode * ctx.time_left;
        return ctx.resources.geode + last_geodes;
    }

    collect_resources(&mut ctx);

    if built {
        update_robots(&mut ctx, &next);
        let mut max_geodes = 0;
        for next in &NEXT_BUILDS {
            let mut new_ctx = ctx.clone();
            new_ctx.time_left -= 1;
            let geodes = recurse(new_ctx, *next);
            max_geodes = max_geodes.max(geodes);
        }

        max_geodes
    } else {
        let mut new_ctx = ctx.clone();
        new_ctx.time_left -= 1;
        recurse(new_ctx, next)
    }
}

fn enough_time(ctx: &Ctx, next: &[u32; 4]) -> bool {
    match *next {
        [1, 0, 0, 0] => {
            let needed_ore = ctx.blueprint.ore - ctx.resources.ore;
            let needed_time = 1 + (needed_ore as f32 / ctx.robots.ore as f32).ceil() as u32;
            needed_time <= ctx.time_left
        }
        [0, 1, 0, 0] => {
            let needed_ore = ctx.blueprint.clay - ctx.resources.ore;
            let needed_time = 1 + (needed_ore as f32 / ctx.robots.ore as f32).ceil() as u32;
            needed_time <= ctx.time_left
        }
        [0, 0, 1, 0] => {
            let needed_ore = ctx.blueprint.obsidian.0.saturating_sub(ctx.resources.ore);
            let needed_clay = ctx.blueprint.obsidian.1.saturating_sub(ctx.resources.clay);
            let ore_time = (needed_ore as f32 / ctx.robots.ore as f32).ceil() as u32;
            let clay_time = (needed_clay as f32 / ctx.robots.clay as f32).ceil() as u32;
            let needed_time = 1u32.saturating_add(ore_time.max(clay_time));
            needed_time <= ctx.time_left
        }
        [0, 0, 0, 1] => {
            let needed_ore = ctx.blueprint.geode.0.saturating_sub(ctx.resources.ore);
            let needed_obs = ctx.blueprint.geode.1.saturating_sub(ctx.resources.obsidian);
            let ore_time = (needed_ore as f32 / ctx.robots.ore as f32).ceil() as u32;
            let obs_time = (needed_obs as f32 / ctx.robots.obsidian as f32).ceil() as u32;
            let needed_time = 1u32.saturating_add(ore_time.max(obs_time));
            needed_time <= ctx.time_left
        }
        _ => unreachable!(),
    }
}

fn update_robots(ctx: &mut Ctx, next: &[u32; 4]) {
    match *next {
        [1, 0, 0, 0] => {
            ctx.robots.ore += 1;
        }
        [0, 1, 0, 0] => {
            ctx.robots.clay += 1;
        }
        [0, 0, 1, 0] => {
            ctx.robots.obsidian += 1;
        }
        [0, 0, 0, 1] => {
            ctx.robots.geode += 1;
        }
        _ => unreachable!(),
    }
}

fn collect_resources(ctx: &mut Ctx) {
    ctx.resources.ore += ctx.robots.ore;
    ctx.resources.clay += ctx.robots.clay;
    ctx.resources.obsidian += ctx.robots.obsidian;
    ctx.resources.geode += ctx.robots.geode;
}

fn start_build(ctx: &mut Ctx, next: &[u32; 4]) {
    match *next {
        [1, 0, 0, 0] => {
            ctx.resources.ore -= ctx.blueprint.ore;
        }
        [0, 1, 0, 0] => {
            ctx.resources.ore -= ctx.blueprint.clay;
        }
        [0, 0, 1, 0] => {
            ctx.resources.ore -= ctx.blueprint.obsidian.0;
            ctx.resources.clay -= ctx.blueprint.obsidian.1;
        }
        [0, 0, 0, 1] => {
            ctx.resources.ore -= ctx.blueprint.geode.0;
            ctx.resources.obsidian -= ctx.blueprint.geode.1;
        }
        _ => unreachable!(),
    }
}

fn enough_resources(resources: &Counters, blueprint: &Blueprint, next: &[u32; 4]) -> bool {
    match *next {
        [1, 0, 0, 0] => resources.ore >= blueprint.ore,
        [0, 1, 0, 0] => resources.ore >= blueprint.clay,
        [0, 0, 1, 0] => {
            resources.ore >= blueprint.obsidian.0 && resources.clay >= blueprint.obsidian.1
        }
        [0, 0, 0, 1] => {
            resources.ore >= blueprint.geode.0 && resources.obsidian >= blueprint.geode.1
        }
        _ => unreachable!(),
    }
}

fn main() {
    let handle = io::stdin().lock();
    let mut blueprints = vec![];
    for line in handle.lines().map(Result::unwrap).take(3) {
        let costs: Vec<_> = line
            .split(&[':', '.'])
            .skip(1)
            .take(4)
            .map(|l| {
                l.split_whitespace()
                    .filter_map(|l| l.parse::<u32>().ok())
                    .collect::<Vec<_>>()
            })
            .collect();
        blueprints.push(Blueprint {
            ore: costs[0][0],
            clay: costs[1][0],
            obsidian: (costs[2][0], costs[2][1]),
            geode: (costs[3][0], costs[3][1]),
        });
    }

    let result: u32 = blueprints
        .into_par_iter()
        .map(|blueprint| {
            let ctx = Ctx {
                blueprint,
                robots: Counters {
                    ore: 1,
                    ..Default::default()
                },
                resources: Default::default(),
                time_left: 32,
            };

            let max_geodes: u32 = NEXT_BUILDS
                .par_iter()
                .map(|next| {
                    let new_ctx = ctx.clone();
                    recurse(new_ctx, *next)
                })
                .max()
                .unwrap();

            println!("Ran blueprint. Max geodes: {max_geodes}");
            max_geodes
        })
        .product();
    dbg!(result);
}
