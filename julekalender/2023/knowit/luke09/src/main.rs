use std::fs;
use std::collections::HashMap;

fn main() {
    let rekke: Vec<(u32, u32)> = fs::read_to_string("src/rekke.txt")
        .expect("hmm")
        .trim()
        .split("), (")
        .map(|s| {
            let mut parts = s.split(", ");
            let first = parts.next().unwrap_or("0").parse().unwrap_or(0);
            let second = parts.next().unwrap_or("0").parse().unwrap_or(0);
            (first, second)
        })
        .collect();
    // println!(":?}", rekke);

    let mut connections: HashMap<u32, Vec<u32>> = HashMap::new(); 
    for (point1, point2) in rekke {
        connections.entry(point1).or_insert_with(Vec::new).push(point2);
        connections.entry(point2).or_insert_with(Vec::new).push(point1);
    }
    // println!("{:?}", connections);
    
    let end_points: Vec<u32> = connections
        .iter()
        .filter(|(_, connected_points)| connected_points.len() == 1)
        .map(|(&point, _)| point)
        .collect();
    // println!("{:?}", end_points); // The order of this is random, which means that
                                     // The code panics 50% of the timt lol

    let mut path: Vec<u32> = Vec::new();
    let mut current_point = end_points[0];
    // let mut prev_point = 0;
    while current_point != end_points[1] {
        let mut next_point: Vec<u32> = connections.get_mut(&current_point).unwrap().clone();
        next_point.retain(|&item| !path.iter().any(|&i| i==item));
        // prev_point = current_point;
        current_point = next_point[0];
        path.push(current_point);
    }
    println!("{:?}", path);
    let len = path.len();
    let mid = (len as f32 / 2.0).floor() as usize;
    let mid_sum = path[mid-1] + path[mid];
    println!("{}", mid_sum);
}
