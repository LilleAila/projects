fn main() {
    let d4: Vec<usize> = std::fs::read_to_string("src/d/d4.txt")
        .expect("whoops")
        .lines()
        .map(|l| l.trim().parse().unwrap_or(0))
        .collect();
    let d6: Vec<usize> = std::fs::read_to_string("src/d/d6.txt")
        .expect("whoops")
        .lines()
        .map(|l| l.trim().parse().unwrap_or(0))
        .collect();
    let d8: Vec<usize> = std::fs::read_to_string("src/d/d8.txt")
        .expect("whoops")
        .lines()
        .map(|l| l.trim().parse().unwrap_or(0))
        .collect();
    let d10: Vec<usize> = std::fs::read_to_string("src/d/d10.txt")
        .expect("whoops")
        .lines()
        .map(|l| l.trim().parse().unwrap_or(0))
        .collect();
    let d20: Vec<usize> = std::fs::read_to_string("src/d/d20.txt")
        .expect("whoops")
        .lines()
        .map(|l| l.trim().parse().unwrap_or(0))
        .collect();

    let mut d4_idx: usize = 0; // When reading, do % rolls.len()
    let mut d6_idx: usize = 0;
    let mut d8_idx: usize = 0;
    let mut d10_idx: usize = 0;
    let mut d20_idx: usize = 0;

    let mut num_attacks: usize = 0;
    let mut julenisse: isize = 10000000;
    let julenisse_acc: usize = 18;
    while julenisse > 0 {
        num_attacks += 1;
        if num_attacks % 3 == 0 {
            let acc = d20[d20_idx % d20.len()] + 3;
            d20_idx += 1;
            if acc < julenisse_acc { continue }
            let attack =
                std::cmp::min(d10[d10_idx % d10.len()], d10[(d10_idx + 1)%d10.len()])
                + 6;
            d10_idx += 2;
            julenisse -= attack as isize;
        }
        else if (num_attacks - 1) % 3 == 0 {
            let acc = d20[d20_idx%d20.len()] + 6;
            d20_idx += 1;
            if acc < julenisse_acc { continue }
            let attack = d8[d8_idx % d8.len()] + 5;
            d8_idx += 1;
            julenisse -= attack as isize;
        }
        else {
            let acc = d20[d20_idx % d20.len()] + 8;
            d20_idx += 1;
            // Hvis feil svar, kanskje attack også må regnes ut
            // uansett om den treffer eller ikke
            let attack =
                std::cmp::max(d6[d6_idx%d6.len()], d6[(d6_idx+1)%d6.len()])
                + 2 + d4[d4_idx % d4.len()];
            d6_idx += 2;
            d4_idx += 1;
            if acc < julenisse_acc { continue }
            julenisse -= attack as isize;
        }
    }
    // After each use, the d variables increase by 1
    // If accuracy is less than 18, attack failed
    // Attack n1: 2d6K1 + 2 + 1d4
    // 2 * d6 (take highest) + 2 + d4
    // Accuracy n1: 1d20 + 8
    // d20 + 8
    //
    // Attack n2: 1d8 + 5
    // d8 + 5
    // Accuracy n2: 1d20 + 6
    // d20 + 6
    //
    // Attack n2: 2d10KL1 + 6
    // 2 * d10 (take lowest) + 6
    println!("{}", num_attacks);
}
