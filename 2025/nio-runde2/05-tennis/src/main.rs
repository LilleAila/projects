use std::io::stdin;

#[derive(Debug)]
enum Player {
    A,
    B,
}

type Game = (i32, i32);
type GameResult = Option<(Player, i32)>;

fn play_game(points: &Vec<Player>, &game: &Game) -> GameResult {
    let (required_points, required_sets) = game;
    let mut total_score = 0;
    let mut score = (0, 0);
    let mut sets = (0, 0);
    for p in points {
        match p {
            Player::A => score.0 += 1,
            Player::B => score.1 += 1,
        }
        total_score += 1;
        if score.0 >= required_points && score.0 >= score.1 + 2 {
            // A won
            sets.0 += 1;
            score = (0, 0);
            if sets.0 >= required_sets {
                return Some((Player::A, total_score));
            }
        }
        else if score.1 >= required_points && score.1 >= score.0 + 2 {
            // B won
            sets.1 += 1;
            score = (0, 0);
            if sets.1 >= required_sets {
                return Some((Player::B, total_score));
            }
        }
    }
    None
}

fn main() {
    let stdin = stdin();

    let mut input = String::new();
    stdin.read_line(&mut input).unwrap();
    let mut input = input.trim().split_whitespace();

    let _num_points: i32 = input.next().unwrap().parse().unwrap();
    // dbg!(&num_points);
    let num_games: i32 = input.next().unwrap().parse().unwrap();
    // dbg!(&num_games);

    let mut input = String::new();
    stdin.read_line(&mut input).unwrap();
    let points: Vec<Player> = input.chars().filter_map(|c| match c {
        'A' => Some(Player::A),
        'B' => Some(Player::B),
        _ => None,
    }).collect();
    // dbg!(&points);

    let mut games: Vec<Game> = Vec::with_capacity(num_games as usize);
    for _ in 0..num_games {
        let mut input = String::new();
        stdin.read_line(&mut input).unwrap();
        let mut input = input.trim().split_whitespace();
        games.push((input.next().unwrap().parse().unwrap(), input.next().unwrap().parse().unwrap()));
    }
    // dbg!(&games);

    for game in &games {
        let result = play_game(&points, game);
        match result {
            Some((winner, score)) => println!("{:?} {}", winner, score),
            None => println!("X"),
        }
    }
}
