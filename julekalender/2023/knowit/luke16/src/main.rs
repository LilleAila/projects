fn main() {
    // let input: Vec<usize> = vec![4, 2, 1, 5, 2, 3, 2, 1];
    
    let input: Vec<usize> = std::fs::read_to_string("src/rekke.txt")
        .expect("hmm")
        .trim()
        .trim_end_matches(']')
        .trim_start_matches('[')
        .split(", ")
        .map(|s| s.parse().unwrap_or(0))
        .collect();
    
    let mut output: Vec<usize> = vec![1; input.len()];
    let mut prev_output: Vec<usize> = vec![0; input.len()];

    while output != prev_output {
        prev_output = output.clone();
        for i in 0..output.len() {
            let alv_val: (usize, usize, usize) = (input[i], output[i], i);
            let (alv_years, alv_money, alv_index) = alv_val;
            let mut neighbors: Vec<(usize, usize)> = Vec::new();
            if i > 1 { neighbors.push((input[i-2], output[i-2])) }
            if i > 0 { neighbors.push((input[i-1], output[i-1])) }
            if i < output.len()-1 { neighbors.push((input[i+1], output[i+1])) }
            if i < output.len()-2 { neighbors.push((input[i+2], output[i+2])) }
            // println!("{:?}, {:?}", alv_val, neighbors);
            let mut add_money = false;
            for neighbor in neighbors {
                let (years, money) = neighbor;
                // println!("{:?}", neighbor);
                if alv_years > years && alv_money <= money {
                    add_money = true;
                } else if alv_years == years && alv_money < money {
                    add_money = true;
                }
            }
            if add_money { output[alv_index] = alv_money + 1 }
        }
    }

    // println!("{:?}", output);
    println!("{}", output.iter().sum::<usize>());
}
