fn main() {
    let mut num_numbers: i32 = 0;
    let mut numbers_str: String = String::new();
    let max_val: i32 = 100000;
    for i in 0..max_val { // Alle UNDER 100 000
        let item_string: String = i.to_string();
        if !numbers_str.contains(&item_string) {
            num_numbers += 1;
            numbers_str.push_str(&item_string);
        }
    }
    println!("Antall tall: {}", num_numbers);
}
