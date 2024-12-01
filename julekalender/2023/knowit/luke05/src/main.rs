fn get_sum_div(num: f64) -> f64 {
    let num_sum: f64 = num.to_string()
        .chars()
        .filter_map(|c| c.to_digit(10))
        .sum::<u32>() as f64;

    let divided: f64 = num / num_sum;

    // println!("{} {} {}", num, num_sum, divided);

    if divided % 1.0 > 0.0 { return -1.0 }

    return divided;
}

fn is_prime(n: f64) -> bool {
    if n <= 1.0{
        return false;
    }
    if n <= 3.0 {
        return true;
    }
    if n % 2.0 == 0.0 || n % 3.0 == 0.0 {
        return false;
    }

    let mut i = 5.0;
    while i * i <= n {
        if n % i == 0.0 || n % (i + 2.0) == 0.0 {
            return false;
        }
        i += 6.0;
    }

    true
}

fn check_prime_div(num: f64) -> bool {
    let sum_div: f64 = get_sum_div(num);
    if sum_div < 0.0 { return false };
    return is_prime(sum_div as f64);
}

fn main() {
    let min_num: i32 = 1;
    let max_num: i32 = 100000000;
    // let max_num: i32 = 100;
    let mut num_primes: i32 = 0;
    for i in (min_num)..(max_num + 1) {
        let checked: bool = check_prime_div(i as f64);
        if checked {
            num_primes += 1;
            // println!("{}", i);
        }
        if i % 5000000 == 0 { println!("{}", i) }
    }

    println!("Antall sum-dele-primtall: {}", num_primes);
    // 6k±1 prime alg: 1020748
    // primes library: 1020748 <--
    // One-line from SO: 1022242
    // Simple: 1020757
    // 2 is not prime: 1020747
    // 1 is prime: 1020757
    // Rounded: 7182262
    // Ceiled: 7235739
    // Floored: 7233747
    // Without decimal check: 7233747
    // Change everything to use flats: 642344 (correct)

    // println!("{}", check_prime_div(9015));
    // println!("{}", check_prime_div(18));
}
