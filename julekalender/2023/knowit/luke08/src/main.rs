use std::fs;

fn main() {
    let alphabet: [char; 29] = "abcdefghijklmnopqrstuvwxyzæøå"
        .chars()
        .collect::<Vec<char>>()
        .try_into()
        .expect("Error3");
    // Removed [ and ] from input file to make ti easier to parse
    let key: Vec<Vec<usize>> = fs::read_to_string("src/input.txt")
        .expect("Erro")
        .lines()
        .map(|l| {
            l.split(", ")
                .map(|c| c.parse()
                     .unwrap_or(0))
                .collect()
                // .try_into()
                // .expect("Error2")
        })
        .collect();

    let dikt: Vec<Vec<String>> = fs::read_to_string("src/cypher.txt")
        .expect("Error4")
        .lines()
        .map(|l| l.split_whitespace().map(|s| s.to_string()).collect())
        // .split_whitespace()
        // .map(|s| s.to_string())
        .collect::<Vec<Vec<String>>>();
    // println!("{}, {}", dikt.len(), key.len()); // Begge er 129 :)

    let mut output: Vec<Vec<String>> = Vec::new();
    let mut counter: usize = 0;
    for i in 0..dikt.len() {
        output.push(Vec::new());
        for j in 0..dikt[i].len() {
            let curr_word = &dikt[i][j];
            let curr_key = &key[counter];
            let mut temp_word: String = String::new();
            for char in curr_word.chars() {
                if char == ',' { temp_word.push(char) }
                else {
                    let char_idx = alphabet.iter().position(|&c| c==char).unwrap();
                    let char_key_idx = curr_key.iter().position(|&n| n==char_idx).unwrap();
                    let new_char = alphabet[char_key_idx];
                    temp_word.push(new_char);
                }
            }
            counter += 1;
            output[i].push(temp_word);
        }
    }
    // println!("{:?}", output);

    let output_str: String = output
        .iter()
        .map(|l|l.join(" "))
        .collect::<Vec<String>>()
        .join("\n");
    println!("{}", output_str);
}
