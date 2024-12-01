fn decrypt_rot(bokstav: char, n: usize) -> char {
    // Use the reversed alphabet to un-encode
    let alphabet: Vec<char> = "abcdefghijklmnopqrstuvwxyz".chars().rev().collect();
    let initial_index = alphabet.iter().position(|&r| r==bokstav).unwrap();
    alphabet[(initial_index + n)%alphabet.len()]
}

fn get_bin_even() -> Vec<usize> {
    let mut nums: Vec<usize> = Vec::new();
    let mut i = 0;
    while nums.len() < 100 { // Generate one hundred
        let bin = format!("{:b}", i)
            .chars()
            .filter(|&c| c=='1')
            .count();
        if bin as f32 % 2.0 == 0.0 { nums.push(i) }
        i += 1;
    }
    nums
}

fn main() {
    let message: Vec<char> = "Ojfkyezkz bvclae zisj a guomiwly qr tmuematbcqxqv sa zmcgloz."
        .chars()
        .collect();
    let alphabet: Vec<char> = "abcdefghijklmnopqrstuvwxyz".chars().collect();
    let mut output: Vec<char> = Vec::new();
    let x = 666; // No. of lines in src/twin_primes.txt
    let even_bin_nums = get_bin_even();
    for i in 0..message.len() {
        // Tar kun lowercase, O ble den samme uansett
        if !alphabet.iter().any(|&j| j==message[i]) {
            output.push(message[i]);
            continue;
        }
        let y = even_bin_nums[i];
        let decoded = decrypt_rot(message[i], x*y);
        output.push(decoded);
    }
    println!("{:?}", String::from_iter(output.iter()));
}
