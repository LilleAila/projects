use std::collections::HashMap;
use std::io;

fn main() {
    let mut lines = io::stdin().lines();
    lines.next();

    let mut graph: HashMap<i32, Vec<(String, i32)>> = HashMap::new();

    while let Some(l) = lines.next() {
        let l = l.unwrap();
        let ps: [&str; 3] = l.split(";").collect::<Vec<_>>().try_into().unwrap();

        let name = ps[0].to_string();
        let mut birth: i32 = ps[1].parse().unwrap();
        let mut death: i32 = ps[2].parse().unwrap();

        if birth > death {
            birth = -birth;
            death = -death;
        }

        graph.entry(birth).or_default().push((name, death));
    }

    // let start: i32 = graph.keys().min().map(|&x| x).unwrap();
    // let target: i32 = graph.keys().max().map(|&x| x).unwrap();
    let start: i32 = 300;
    let target: i32 = 2025;
    let mut stack: Vec<(i32, Vec<(String, i32)>)> = vec![];
    let mut visited: HashMap<i32, bool> = HashMap::new();
    stack.push((start, Vec::new()));
    visited.insert(start, true);

    while let Some((y, ps)) = stack.pop() {
        if y == target {
            println!("{}", ps.len());
            println!("{:?}", ps);
            break;
        }

        if let Some(edges) = graph.get(&y) {
            for (name, death) in edges {
                if !visited.contains_key(death) {
                    let mut path = ps.clone();
                    path.push((name.clone(), *death));
                    stack.push((*death, path));
                    visited.insert(*death, true);
                }
            }
        }
    }
}
