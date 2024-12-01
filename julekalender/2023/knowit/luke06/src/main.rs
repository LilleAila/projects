use std::fs;

fn main() {
    let byer: Vec<(i32, i32)> = fs::read_to_string("src/rute.txt")
        .unwrap()
        .trim()
        .lines()
        .map(|l| {
                 let vals: Vec<i32> = l.split(",").map(|s| s.parse().unwrap()).collect();
                 (vals[0], vals[1])
             })
        .collect();

    let mut distance_traveled: f32 = 0.0;
    let (mut prev_x, mut prev_y) = byer[0];
    for i in 1..byer.len() {
        let (x, y) = byer[i];
        let x_diff: f32 = (x - prev_x) as f32;
        let y_diff: f32 = (y - prev_y) as f32;
        let dist: f32 = (x_diff*x_diff + y_diff*y_diff).sqrt();//.ceil() as i32;
        distance_traveled += dist;
        (prev_x, prev_y) = (x, y);
    }
    let reinsdyrfor: i32 = (distance_traveled * 9.0 / 1000.0).ceil() as i32;
    println!("Reinsdyrfor: {}", reinsdyrfor);
}
