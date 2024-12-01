use std::fs;

fn main() {
    let aksjer: Vec<Vec<i64>> = fs::read_to_string("src/input.txt")
        .expect("error")
        .split("\n")
        .map(|line| line.split(",")
             .map(|s| s.parse().unwrap_or(0)).collect()
        )
        .collect();

    let mut total_money: i64 = 200000;
    for dag in &aksjer {
        if dag.len() == 1 { continue }

        let mut max_diff = 0;
        let mut optimal: (i64, i64) = (0, 0);
        for i in 0..dag.len()-1 {
            for j in i+1..dag.len() {
                let diff = dag[j] - dag[i];
                if diff > max_diff {
                    optimal = (dag[i], dag[j]);
                    max_diff = diff;
                }
            }
        }

        let (buy, sell): (i64, i64) = optimal;
        let bought_num: i64 = total_money / buy; // Automatically rounded down because int
        let bought_price: i64 = bought_num * buy;
        let sold_price: i64 = bought_num * sell;
        total_money = total_money - bought_price + sold_price;
    }
    println!("Total penger: {}", total_money);
}
