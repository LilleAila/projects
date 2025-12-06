use std::io::stdin;
use std::cmp::Ordering;
use std::collections::HashMap;

fn parse_block(input: &str) -> (i32, i32) {
    let mut numbers = input
        .trim()
        .split_whitespace()
        .map(|num| num.parse::<i32>().ok());

    (numbers.next().unwrap().unwrap(), numbers.next().unwrap().unwrap())
}

fn main() {
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    let number_of_blocks: i32 = input.trim().parse().unwrap();
    // dbg!(&number_of_blocks);

    let mut blocks: Vec<(i32, i32)> = Vec::new();
    for _ in 0..number_of_blocks {
        let mut input = String::new();
        stdin().read_line(&mut input).unwrap();
        let block = parse_block(&input);
        blocks.push(block);
    }
    // dbg!(&blocks);

    blocks.sort_by(|&(_, b1), &(_, b2)| {
        if b1 < b2 {
            Ordering::Greater
        } else if b2 < b1 {
            Ordering::Less
        } else {
            Ordering::Equal
        }
    });
    // dbg!(&blocks);

    let mut increasing_blocks: HashMap<i32, (i32, i32)> = HashMap::new();

    for (h, b) in blocks {
        increasing_blocks.entry(b)
            .and_modify(|entry| {
                if h > entry.0 {
                    *entry = (h, b);
                }
            })
            .or_insert((h, b));
    }

    let increasing_blocks: Vec<(i32, i32)> = increasing_blocks.into_values().collect();
    // dbg!(&increasing_blocks);

    let mut tower: i32 = 0;

    for (h, _) in &increasing_blocks {
        tower += h;
    }

    println!("{}", tower);
}
