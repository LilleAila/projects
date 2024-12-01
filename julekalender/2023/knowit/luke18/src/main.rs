fn main() {
    let mut total_penger = 0;
    for current_file in 1..=10 {
        let file: Vec<String> = std::fs::read_to_string(format!("src/graphs/graph_{}.txt", current_file))
            .expect("oops")
            .lines()
            .map(|a| a.to_string())
            .collect();
        let graph_in: Vec<Vec<char>> = file[0..file.len()-2].to_vec()
            .iter()
            .map(|a| a.chars().collect())
            .collect();
        // Bool is true if bought, false if sold (buy status)
        // isize is the current price
        let graph: Vec<(bool, isize)> = parse_graph(&graph_in);
        let values: Vec<isize> = file[file.len()-1].clone().split(", ")
            .map(|n| n.parse().unwrap_or(0)).collect();
        // println!("{:?}, {:?}", graph.len(), values.len());

        let mut penger: isize = 0;

        for i in 0..graph.len() {
            let (bought, price) = graph[i];
            let amount = values[i];
            if bought {
                penger -= price * amount;
            } else {
                penger += price * amount;
            }
        }

        // println!("{}", penger);
        total_penger += penger;
    }
    println!("{}", total_penger);
}

fn parse_graph(graph: &Vec<Vec<char>>) -> Vec<(bool, isize)> {
    let rows = graph.len();
    let cols = graph[0].len();

    let mut parsed_graph = vec![vec![' '; rows]; cols];

    for i in 0..rows {
        for j in 0..cols {
            let value = graph[i][j];
            let destination = rows - 1 - i;
            parsed_graph[j][destination] = value;
        }
    }

    parsed_graph.retain(|row| row.iter().any(|&c| c == 'S' || c == 'K'));

    let mut buy_sell_points: Vec<(bool, isize)> = Vec::new(); // Vector of bought/sold, and
                                                             // index(price)
    for transaction in parsed_graph {
        if transaction.iter().any(|&c| c=='S') {
            let price: isize = transaction.iter().position(|&c| c == 'S').unwrap_or(0) as isize;
            buy_sell_points.push((false, price));
        } else {
            let price: isize = transaction.iter().position(|&c| c == 'K').unwrap_or(0) as isize;
            buy_sell_points.push((true, price));
        }
    }

    buy_sell_points
}
