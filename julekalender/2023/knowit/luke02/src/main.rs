// Denne koden sjekker alle, men man kunne egentlig bare gjort
// (antall klikk - antal klakk) / 7
// Og flooret hvis det ikke er et heltall
use std::fs;

fn main() {
    let log: Vec<(bool, i32)> = fs::read_to_string("src/log.txt")
        .expect("Error lol")
        .split(", ")
        .map(|s| {
            let parts: Vec<&str> = s.split(" ").collect();
            (
                parts[0].starts_with("klikk"),
                parts[2].trim().parse().unwrap_or(0) - 1
            )
        })
        .collect();

    let mut current_lock: [bool; 7] = [false; 7];
    let mut total_locks = 0;
    for lock in log {
        let (state, pin) = lock;
        current_lock[pin as usize] = state;
        if current_lock.iter().all(|&x| x) {
            total_locks += 1;
            current_lock = [false; 7];
        }
    }
    println!("Total locks: {}", total_locks);
}
