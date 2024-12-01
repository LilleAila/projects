fn validate(card: String) -> bool {
    let card_digits: Vec<char> = card.chars().collect();
    let target: u32 = card_digits.iter().rev().take(2).rev().collect::<String>().parse().unwrap_or(0);
    let sum: u32 = card_digits
        .iter()
        .take(22)
        .map(|c| c.to_digit(10).unwrap_or(0))
        .enumerate()
        .map(|(index, value)| if index % 2 == 0 { value * 2 } else { value })
        .sum();
    let number = (24 - (sum % 24)) % 24;

    if number == target { return true }
    false
}

fn main() {
    let input: Vec<String> = std::fs::read_to_string("src/kredittkort.txt")
        .expect("ops")
        .lines()
        .map(|l| l.to_string())
        .collect();

    // let eksempel: String = "234671631264987292370142".to_string();
    // let valid: bool = validate(eksempel);
    // println!("{}", valid);

    let mut num_false = 0;
    for kort in input {
        if !validate(kort) { num_false += 1 }
    }

    println!("{}", num_false);
}
