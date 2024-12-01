/*
 * Måte å løse oppgaven:
 * Starte med en å finne oppe-venstre hjørne. (GJORT)
 * Scanne rad for rad
 *      Finne ut hvilken brikke som passer videre med den høyre siden (medregnet rotasjon)
 *      Rotere brikker med 0, 1, 2 eller 3 etter hvor de passer
 * Etter en rad er scannet, finn brikke som passer under første brikke i raden.
 * Kjør i en while-loop, og break når enten det er like mange brikker i løsning som input,
 *  eller til man kommer til den fjerde (nede til høyre) hjørne-brikken.
 * */

fn main() {
    let puzzle: Vec<(String, Vec<isize>, usize)> = std::fs::read_to_string("src/puzzle.txt")
        .expect("hmm")
        .lines()
        .map(|l| {
            let parts: Vec<&str> = l.trim().split(", [").collect();
            let numbers: Vec<isize> = parts[1]
                .trim_end_matches(']')
                .split(", ")
                .map(|n| n.trim().parse().unwrap_or(0))
                .collect();
            (
                parts[0].to_string(),
                numbers,
                0
            )
        })
        .collect();

    // Find a the target sum and a corner piece to start with.
    let mut target_sum = 0; // 2599
    let mut current_piece: (String, Vec<isize>, usize) = puzzle[0].clone();
    for piece in &puzzle {
        for &num in &piece.1 {
            if num > target_sum { target_sum = num }
        }

        if piece.1.iter().filter(|&&x| x == -1).count() == 2 {
            current_piece = piece.clone();
        }
    }

    // println!("{:?}", current_piece);

    // 2d vector of (filename, rotation)
    // It is initialized with the starting corner piece in the top left
    let mut solved_puzzle: Vec<Vec<(String, Vec<isize>, usize)>> =
                            vec![vec![current_piece.clone()]];
    let mut current_row = 0;
    // Up, Right, Down, Left (Clockwise)
    // The first found item is edge on up and left.

    loop {
        // Scan to the right until it hits an edge, potentially rotating the new piece
        current_piece = solved_puzzle[solved_puzzle.len()-1][solved_puzzle[solved_puzzle.len()-1].len()-1].clone();
        if solved_puzzle[current_row].len() > 0 {
            let (_, sides, rotation) = current_piece.clone();
            let next_value = target_sum - sides[(1 + rotation) % sides.len()];
            for piece in &puzzle {
                let (next_name, next_sides, _) = piece.clone();
                if next_sides.iter().any(|&i| i==next_value) {
                    // Find out if it has to be rotated
                    let idx = next_sides.iter().position(|&x| x == next_value).unwrap();
                    let next_rotation = (idx + 3) % 4;
                    solved_puzzle[current_row].push((next_name, next_sides.clone(), next_rotation));

                    if next_sides.iter().filter(|&&x| x==-1).count() ==
                        solved_puzzle[current_row][0].1.iter().filter(|&&y| y==-1).count() {
                            current_row += 1;
                            solved_puzzle.push(Vec::new());
                        }
                }
            }
        } else {
            let (_, sides, rotation) = solved_puzzle[current_row - 1][0].clone();
            let next_value = target_sum - sides[(2 + rotation) % sides.len()];
            for piece in &puzzle {
                let (next_name, next_sides, _) = piece.clone();
                if next_sides.iter().any(|&i| i==next_value) {
                    let idx = next_sides.iter().position(|&x| x== next_value).unwrap();
                    let next_rotation = (idx + 2) % 4;
                    solved_puzzle[current_row].push((next_name, next_sides, next_rotation));
                }
            }
        }

        if current_piece.1.iter().filter(|&&x| x==-1).count() >= 2 {
            break;
        }
    }

    // println!("Highest number: {}", target_sum);
    println!("{:?}", solved_puzzle);
}
