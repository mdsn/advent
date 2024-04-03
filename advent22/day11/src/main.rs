use std::collections::VecDeque;
use std::io;
use std::io::prelude::*;

#[derive(Debug, Clone)]
enum Operation {
    Add(u64),
    Mul(u64),
    Square,
}

impl Operation {
    fn apply(&self, item: &Item) -> u64 {
        match self {
            Operation::Add(x) => x + item.0,
            Operation::Mul(x) => x * item.0,
            Operation::Square => item.0 * item.0,
        }
    }
}

#[derive(Debug, Clone)]
struct Test {
    arg: u64,
    monkey1: usize,
    monkey2: usize,
}

impl Test {
    fn apply(&self, item: &Item) -> usize {
        if item.0 % self.arg == 0 {
            self.monkey1
        } else {
            self.monkey2
        }
    }
}

#[derive(Debug, Clone)]
struct Item(u64);

#[derive(Debug, Clone)]
struct Monkey {
    inspected: u64,
    items: VecDeque<Item>,
    operation: Operation,
    test: Test,
}

impl Monkey {
    fn new(items: VecDeque<Item>, operation: Operation, test: Test) -> Self {
        Monkey {
            inspected: 0,
            items,
            operation,
            test,
        }
    }
}

fn parse_monkey(lines: &[String]) -> Monkey {
    // "  Starting items: 66, 71, 94",
    let (_, items) = lines[1].split_once(':').unwrap();
    let items: VecDeque<Item> = items
        .split(',')
        .map(|i| Item(i.trim().parse().unwrap()))
        .collect();

    // "  Operation: new = old * 5",
    let (_, op) = lines[2].split_once("= ").unwrap();
    let op: Vec<&str> = op.split(' ').skip(1).collect();
    let op = match (op[0], op[1]) {
        ("+", arg) => Operation::Add(arg.parse().unwrap()),
        ("*", "old") => Operation::Square,
        ("*", arg) => Operation::Mul(arg.parse().unwrap()),
        _ => unreachable!(),
    };

    // Test: divisible by 3
    // If true: throw to monkey 7
    // If false: throw to monkey 4
    let test = Test {
        arg: lines[3].split(' ').find_map(|i| i.parse().ok()).unwrap(),
        monkey1: lines[4].split(' ').find_map(|i| i.parse().ok()).unwrap(),
        monkey2: lines[5].split(' ').find_map(|i| i.parse().ok()).unwrap(),
    };

    Monkey::new(items, op, test)
}

fn main() {
    let handle = io::stdin().lock();

    let mut monkeys: Vec<Monkey> = vec![];
    let lines: Vec<_> = handle
        .lines()
        .map(Result::unwrap)
        .filter(|l| !l.is_empty())
        .collect();
    for input in lines.chunks(6) {
        monkeys.push(parse_monkey(input));
    }

    let lcm: u64 = monkeys.iter().map(|m| m.test.arg).product();

    for _ in 0..10000 {
        for m in 0..monkeys.len() {
            let mut monkey = monkeys[m].clone();
            for _ in 0..monkey.items.len() {
                let mut item = monkey.items.pop_front().unwrap();
                item.0 = monkey.operation.apply(&item) % lcm;
                let target = monkey.test.apply(&item);
                monkeys[target].items.push_back(item);
                monkey.inspected += 1;
            }
            monkeys[m] = monkey;
        }
    }

    monkeys.sort_by_key(|m| m.inspected);
    let monkey_business: u64 = monkeys.iter().rev().take(2).map(|m| m.inspected).product();
    dbg!(monkey_business);
}
