use std::fs;

fn main() {
    let reps: Vec<i32> = fs::read_to_string("src/reps.txt")
        .expect("Error lol nah")
        .trim()
        .split(", ")
        .map(|s| s.parse().unwrap_or(0))
        .collect();
    // println!("{:?}", reps);

    // Splits input into lists of increasing values
    let mut inc_lists: Vec<Vec<i32>> = Vec::new();
    let mut temp_list: Vec<i32> = Vec::new();
    let mut prev_rep: i32 = 0;
    for rep in reps {
        if temp_list.len() > 0 && rep <= prev_rep {
            inc_lists.push(temp_list);
            temp_list = Vec::new();
        }
        if rep > prev_rep || temp_list.len() == 0 {
            temp_list.push(rep);
        }
        prev_rep = rep;
    }

    // println!("{:?}", inc_lists);

    let mut largest_sum: i32 = 0;
    let mut largest_len: i32 = 0;
    for list in inc_lists {
        let len: i32 = list.len() as i32;
        let sum: i32 = list.iter().sum();
        if len > largest_len {
            largest_len = len;
            largest_sum = sum;
        } else if len == largest_len && sum > largest_sum {
            largest_sum = sum;
        }
    }

    println!("Største sum: {}", largest_sum);
}
