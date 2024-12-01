fn check(input: Vec<String>) -> bool {
    let alphabet: Vec<char> = "abcdefghijklmnopqrstuvwxyzæøå".chars().collect();

    let name = input[0].clone();
    let price = input[1].clone().parse().unwrap_or(0);
    let hash = input[2].clone().parse().unwrap_or(0);

    let mut sum_letters: usize = 0;
    for char in name.chars() {
        let char_lower = char.to_lowercase().next().unwrap();
        if !alphabet.iter().any(|&i| i==char_lower) { continue }
        sum_letters += alphabet.iter().position(|&x| x == char_lower).unwrap() + 1;
    }

    let out_hash = sum_letters * price % 48879;
    if hash == out_hash { return true }

    false
}

fn main() {
    let input: Vec<Vec<String>> = std::fs::read_to_string("src/transaksjoner.txt")
        .expect("nei")
        .lines()
        .map(|l| l.split(";").map(String::from).collect())
        .collect();

    let mut out_string: String = String::new();
    for item in input {
        if !check(item.clone()) {
            out_string.push(item[0].chars().next().unwrap().to_lowercase().next().unwrap());
        }
    }
    println!("{}", out_string);
}
