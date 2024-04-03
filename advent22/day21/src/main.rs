use std::collections::{HashMap, HashSet, VecDeque};
use std::io::prelude::*;

#[derive(Debug)]
enum Monkey {
    K(u64),
    Add(String, String),
    Sub(String, String),
    Mul(String, String),
    Div(String, String),
}

impl Monkey {
    fn is_k(&self) -> bool {
        matches!(self, Monkey::K(_))
    }

    fn needs(&self, name: &str) -> bool {
        match self {
            Monkey::K(_) => false,
            Monkey::Add(a, b) | Monkey::Sub(a, b) | Monkey::Mul(a, b) | Monkey::Div(a, b) => {
                a == name || b == name
            }
        }
    }

    fn operands(&self) -> [&String; 2] {
        assert!(!self.is_k());
        match self {
            Monkey::Add(a, b) | Monkey::Sub(a, b) | Monkey::Mul(a, b) | Monkey::Div(a, b) => [a, b],
            _ => unreachable!(),
        }
    }
}

fn main() {
    let monkeys: HashMap<String, Monkey> = std::io::stdin()
        .lock()
        .lines()
        .map(Result::unwrap)
        .map(|line| {
            let (name, task) = line.split_once(": ").unwrap();
            let op = task
                .parse::<u64>()
                .map(Monkey::K)
                .or_else(|_| {
                    let parts = task.split_whitespace().collect::<Vec<_>>();
                    let (a, b) = (parts[0].to_string(), parts[2].to_string());
                    Ok::<Monkey, ()>(match parts[1] {
                        "+" => Monkey::Add(a, b),
                        "-" => Monkey::Sub(a, b),
                        "*" => Monkey::Mul(a, b),
                        "/" => Monkey::Div(a, b),
                        _ => unreachable!(),
                    })
                })
                .unwrap();
            (name.to_string(), op)
        })
        .collect();

    let starters: HashSet<String> = monkeys
        .iter()
        .filter_map(|(name, op)| op.is_k().then_some(name.to_string()))
        .collect();
    let queue = toposort(&monkeys, starters, "root");
    let part1 = yell(&monkeys, queue);
    dbg!(part1);

    let (lq, rq) = split_branches(&monkeys, "root");
    let (human_branch, monkey_branch) = if lq.contains(&"humn".to_string()) {
        (lq, rq)
    } else {
        (rq, lq)
    };

    let part2 = reverse_engineer(&monkeys, human_branch, yell(&monkeys, monkey_branch));
    dbg!(part2);
}

fn reverse_engineer(monkeys: &HashMap<String, Monkey>, queue: Vec<String>, target: u64) -> u64 {
    // Which of the two branches is the known operand
    enum KnownOp {
        L,
        R,
    }

    assert!(!queue.is_empty());
    let root = queue.last().unwrap();
    let (lq, rq) = split_branches(monkeys, root);
    let (human_branch, monkey_branch, known) = if lq.contains(&"humn".to_string()) {
        (lq, rq, KnownOp::R)
    } else {
        (rq, lq, KnownOp::L)
    };

    let known_operand = yell(monkeys, monkey_branch);
    let root_monkey = &monkeys[root];
    assert!(!root_monkey.is_k());

    let op = match root_monkey {
        Monkey::Add(_, _) => Op::Add,
        Monkey::Sub(_, _) => match known {
            KnownOp::R => Op::Subr,
            KnownOp::L => Op::Subl,
        },
        Monkey::Mul(_, _) => Op::Mul,
        Monkey::Div(_, _) => match known {
            KnownOp::R => Op::Divr,
            KnownOp::L => Op::Divl,
        },
        _ => unreachable!(),
    };

    let new_target = reverse_op(op, target, known_operand);
    if human_branch.len() == 1 {
        // only humn left
        new_target
    } else {
        reverse_engineer(monkeys, human_branch, new_target)
    }
}

fn split_branches(monkeys: &HashMap<String, Monkey>, root: &str) -> (Vec<String>, Vec<String>) {
    let [left, right] = monkeys[root].operands();
    let lq = toposort(monkeys, find_starters(monkeys, left), left);
    let rq = toposort(monkeys, find_starters(monkeys, right), right);
    (lq, rq)
}

#[derive(Debug)]
enum Op {
    Add,
    Mul,
    Divl, // known operand on left
    Divr, // known operand on right
    Subl, // known operand on left
    Subr, // known operand on right
}

fn reverse_op(op: Op, target: u64, operand: u64) -> u64 {
    match op {
        Op::Add => target - operand,  // Add(x, 3) = 10 -> 10 - 3 = 7
        Op::Mul => target / operand,  // Mul(2, x) = 16 -> 16 / 2 = 8
        Op::Divl => operand / target, // Divl(12, x) = 3 -> 12 / 3 = 4
        Op::Divr => target * operand, // Divr(x, 4) = 3  -> 3 * 4 = 12
        Op::Subl => operand - target, // Subl(5, x) = 2  -> 5 - 2 = 3
        Op::Subr => target + operand, // Subr(x, 3) = 9  -> 9 + 3 = 12
    }
}

fn yell(monkeys: &HashMap<String, Monkey>, queue: Vec<String>) -> u64 {
    let root = &queue.last().unwrap();
    let mut numbers: HashMap<&String, u64> = [].into();
    for name in &queue {
        let monkey = &monkeys[name];
        if let Monkey::K(k) = monkey {
            numbers.insert(name, *k);
        } else {
            let [a, b] = monkey.operands();
            let (a, b) = (numbers[a], numbers[b]);
            let k = match monkey {
                Monkey::Add(_, _) => a + b,
                Monkey::Sub(_, _) => a - b,
                Monkey::Mul(_, _) => a * b,
                Monkey::Div(_, _) => a / b,
                _ => unreachable!(),
            };
            numbers.insert(name, k);
        }
    }
    numbers[*root]
}

fn find_starters(monkeys: &HashMap<String, Monkey>, root: &str) -> HashSet<String> {
    let mut starters: HashSet<String> = [].into();
    let mut queue: VecDeque<&str> = [root].into();
    while !queue.is_empty() {
        let name = queue.pop_front().unwrap().to_string();
        let monkey = &monkeys[&name];

        if monkey.is_k() {
            starters.insert(name);
            continue;
        }

        for op in &monkey.operands() {
            if monkeys[*op].is_k() {
                starters.insert(op.to_string());
            } else {
                queue.push_back(*op);
            }
        }
    }
    starters
}

fn toposort(
    monkeys: &HashMap<String, Monkey>,
    mut starters: HashSet<String>,
    root: &str,
) -> Vec<String> {
    let mut queue = vec![];
    loop {
        let ready = starters.iter().next().cloned().unwrap();
        starters.remove(&ready);
        queue.push(ready.clone());

        if ready == root {
            break;
        }

        let (name, monkey) = monkeys
            .iter()
            .find(|(_, monkey)| monkey.needs(&ready))
            .unwrap();

        assert!(!monkey.is_k());
        match monkey {
            Monkey::Add(a, b) | Monkey::Sub(a, b) | Monkey::Mul(a, b) | Monkey::Div(a, b) => {
                if queue.contains(a) && queue.contains(b) {
                    starters.insert(name.to_string());
                }
            }
            Monkey::K(_) => unreachable!(),
        }
    }
    queue
}
