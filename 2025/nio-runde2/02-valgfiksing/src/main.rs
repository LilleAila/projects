use std::io::stdin;
use std::collections::HashMap;

#[derive(Debug)]
enum Vote {
    Grytekluter,
    Spikkekniv,
}

#[derive(Debug)]
struct Node {
    vote: Vote,
    cost: i32,
    children: Vec<usize>,
}

fn main() {
    let mut ansatte = String::new();
    stdin().read_line(&mut ansatte).unwrap();
    let ansatte: i32 = ansatte.trim().parse().unwrap();

    let mut tree: Vec<Node> = Vec::new();
    let mut parents: HashMap<usize, Vec<usize>> = HashMap::new();

    // 0 is the top
    for worker in 1..ansatte {
        let mut parent = String::new();
        stdin().read_line(&mut parent).unwrap();
        let parent: usize = parent.trim().parse().unwrap();
        dbg!(&parent);
        parents.entry(parent).or_insert(Vec::new()).push(worker.try_into().unwrap());
    }

    dbg!(&parents);

    for worker in 0..ansatte {
        let mut input = String::new();
        stdin().read_line(&mut input).unwrap();
        let input: Vec<&str> = input.trim().split_whitespace().collect();
        let vote: Vote = if input[0] == "G" { Vote::Grytekluter } else { Vote::Spikkekniv };
        let cost: i32 = input[1].parse().unwrap();
        dbg!(&vote, &cost);
        tree.push(Node { vote, cost, children: Vec::new() });
    }

    for (parent, children) in parents {
        for &child in &children {
            tree[parent].children.push(child);
        }
    }

    dbg!(&tree);
}
