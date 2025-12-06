use std::io::stdin;

fn get_diff(squares: &[i32]) -> i32 {
    let mut sorted_squares = squares.to_vec();
    sorted_squares.sort();

    let middle = sorted_squares.len() / 2;
    if sorted_squares.len() % 2 == 0 {
        let median1 = sorted_squares[middle - 1];
        let median2 = sorted_squares[middle];

        let cost1: i32 = sorted_squares.iter().map(|&s| (s - median1).abs()).sum();
        let cost2: i32 = sorted_squares.iter().map(|&s| (s - median2).abs()).sum();

        return cost1.min(cost2)
    } else {
        let median = sorted_squares[middle];
        let cost: i32 = sorted_squares.iter().map(|&s| (s - median).abs()).sum();
        return cost
    }
}

fn main() {
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    let mut input = input.trim().split_whitespace();
    let length: usize = input.next().unwrap().parse().unwrap();
    let max_diff: i32 = input.next().unwrap().parse().unwrap();

    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    let squares: Vec<i32> = input.trim().split_whitespace().map(|s| s.parse().unwrap()).collect();

    // dbg!(&length, &max_diff, &squares);

    let mut max_length = 0;
    for start in 0..length {
        for end in start + 1..length {
            let window = &squares[start..end];
            let diff = get_diff(window);
            if diff <= max_diff {
                let length = window.len();
                if length > max_length {
                    max_length = length;
                }
            }
        }
    }

    // dbg!(&max_length);
    println!("{}", max_length);
}
