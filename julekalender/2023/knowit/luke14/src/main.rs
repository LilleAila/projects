fn main() {
    let pushups: Vec<usize> = std::fs::read_to_string("src/push.txt")
        .expect("hmm")
        .trim()
        .split(", ")
        .map(|s| s.parse().unwrap_or(0))
        .collect();
    // let pushups: Vec<usize> = vec![16, 3, 1, 2, 9, 8, 12, 14, 19, 21, 20, 11, 2, 4, 3, 1, 7, 9];

    let mut going_up: bool = true;
    let mut sequences: Vec<Vec<usize>> = Vec::new();
    let mut current_sequence: Vec<usize> = Vec::new();
    let mut last_item: usize = 0;
    for set in pushups {
        if current_sequence.len() == 0 {
            current_sequence.push(set);
            going_up = true;
            last_item = set;
            continue;
        }

        if set == last_item {
            if !going_up {
                sequences.push(current_sequence);
                going_up = true;
                current_sequence = Vec::new();
                current_sequence.push(set);
            } else {
                current_sequence = Vec::new();
                current_sequence.push(set);
            }
        }

        // println!("{:?}, {}", current_sequence, going_up);

        if going_up {
            if set > last_item {
                current_sequence.push(set);
            } else {
                going_up = false;
                current_sequence.push(set);
                // sequences.push(current_sequence);
                // current_sequence = Vec::new();
                // current_sequence.push(set);
                // going_up = true;
            }
        } else {
            // println!("{}, {}", set, last_item);
            if set < last_item {
                current_sequence.push(set);
            } else {
                sequences.push(current_sequence);
                going_up = true;
                current_sequence = Vec::new();
                current_sequence.push(last_item);
                current_sequence.push(set);
            }
        }

        last_item = set;
    }
    // println!("{:?}", sequences);

    let sums: Vec<usize> = sequences.iter().map(|s| s.iter().sum()).collect();
    let highest_sum: usize = *sums.iter().max().unwrap();
    println!("{}", highest_sum);
}
