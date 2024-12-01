/*
 * Starter med 50 000 sukkerstenger
 * Satser på mål
    * 17.5% rundet av (opp/ned) til heltall
    * Minst mål - 2 mål vinner hvis mål >= 2
 * goals.txt - antall mål i hver kamp
 * bets.txt - antal mål gjettet og hvilken odds
 * Odds er hvor mye innsatsen blir ganget med
 */

// use std::env;
use std::fs;

// fn round(num: f32) -> f32 {}

fn main() {
    let goals: Vec<f32> = fs::read_to_string("src/goals.txt")
        .expect("Error lol")
        .split(",")
        .filter_map(|s| s.parse::<f32>().ok())
        .collect();
    // println!("Goals:\n{:?}", goals);

    let bets: Vec<(f32, f32)> = fs::read_to_string("src/bets.txt")
        .expect("Error lol")
        .trim_matches(|c| c == '[' || c == ']')
        .split("], [")
        .flat_map(|pair| pair.split(", "))
        .filter_map(|num| num.parse::<f32>().ok())
        .collect::<Vec<f32>>()
        .chunks(2)
        .map(|chunk| (chunk[0], chunk[1]))
        .collect();
    // println!("Bets:\n{:?}", bets);

    let mut sukkerstenger: f32 = 50000.0;
    for i in 0..bets.len() {
        let goals_game = goals[i];
        let (bet_goals, bet_odds) = bets[i];
        let current_bet = (sukkerstenger / 100.0 * 17.5).round() as f32;
        let vunnet = (current_bet * bet_odds).round() as f32;
        // sukkerstenger -= current_bet;

        if bet_goals <= goals_game {
            sukkerstenger += vunnet;
        } else {
            sukkerstenger -= current_bet;
        }
    }

    // println!("Sukkerstenger: {}", sukkerstenger);
    let tapt = 50000.0 - sukkerstenger;
    println!("Tapt: {}", tapt)
}
