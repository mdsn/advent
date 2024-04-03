use std::cmp::Ordering;
use std::io;
use std::io::prelude::*;

enum Order {
    Right,
    Wrong,
}

#[derive(PartialEq, Eq)]
enum Item {
    Int(u32),
    List(Vec<Item>),
}

fn singleton(x: u32) -> Item {
    Item::List(vec![Item::Int(x)])
}

fn insert(parent: &mut Item, child: Item) {
    match parent {
        Item::List(ref mut v) => {
            v.push(child);
        }
        _ => unreachable!(),
    }
}

fn parse(s: &str) -> Item {
    let s = &s[1..s.len() - 1];
    let mut item = Item::List(vec![]);
    let mut stack = vec![];
    let mut tokens = String::new();

    for c in s.chars() {
        match c {
            '[' => {
                stack.push(item);
                item = Item::List(vec![]);
            }

            ']' => {
                if !tokens.is_empty() {
                    let int = tokens.parse::<u32>().unwrap();
                    insert(&mut item, Item::Int(int));
                    tokens.clear()
                }
                let mut parent = stack.pop().unwrap();
                insert(&mut parent, item);
                item = parent;
            }

            ',' => {
                if !tokens.is_empty() {
                    let int = tokens.parse::<u32>().unwrap();
                    insert(&mut item, Item::Int(int));
                    tokens.clear()
                }
            }

            _ => {
                tokens.push(c);
            }
        }
    }

    if !tokens.is_empty() {
        let int = tokens.parse::<u32>().unwrap();
        insert(&mut item, Item::Int(int));
    }

    item
}

fn compare(a: &Item, b: &Item) -> Option<Order> {
    match (a, b) {
        (Item::Int(x), Item::Int(y)) if x < y => Some(Order::Right),
        (Item::Int(x), Item::Int(y)) if x > y => Some(Order::Wrong),
        (Item::Int(_), Item::Int(_)) => None,

        (Item::List(_), Item::Int(y)) => compare(a, &singleton(*y)),
        (Item::Int(x), Item::List(_)) => compare(&singleton(*x), b),

        (Item::List(a), Item::List(b)) => {
            for (x, y) in a.iter().zip(b.iter()) {
                if let Some(order) = compare(x, y) {
                    return Some(order);
                }
            }

            if a.len() < b.len() {
                Some(Order::Right)
            } else if a.len() > b.len() {
                Some(Order::Wrong)
            } else {
                None
            }
        }
    }
}

fn main() {
    let handle = io::stdin().lock();
    let mut packets: Vec<_> = handle
        .lines()
        .map(Result::unwrap)
        .filter(|s| !s.is_empty())
        .map(|s| parse(&s))
        .collect();

    let mut indices = 0;
    for (i, pair) in packets.chunks(2).enumerate() {
        match compare(&pair[0], &pair[1]) {
            Some(Order::Right) => {
                indices += i + 1;
            }
            Some(Order::Wrong) => {}
            None => unreachable!(),
        }
    }
    dbg!(indices);

    packets.push(parse("[[2]]"));
    packets.push(parse("[[6]]"));
    packets.sort_by(|a, b| match compare(a, b) {
        Some(Order::Right) => Ordering::Less,
        Some(Order::Wrong) => Ordering::Greater,
        _ => unreachable!(),
    });

    let div1 = parse("[[2]]");
    let div2 = parse("[[6]]");

    indices = 1;
    for (i, item) in packets.iter().enumerate() {
        if *item == div1 || *item == div2 {
            indices *= i + 1;
        }
    }
    dbg!(indices);
}
