// fn flood_fill(grid_input: Vec<Vec<bool>>, x: usize, y: usize) -> (bool, Vec<Vec<bool>>) {
//     let mut grid = grid_input;
//     // If the selected value in the list is false, return false + the grid
//     // If it is true, use the flood fill algorithm to remove the island,
//     // and return true + the grid
//     // Copy flood fill implementation from luke 10
//     if !grid[y][x] { return (false, grid) }
//     (true, grid)
// }

fn flood_fill(grid_input: Vec<Vec<bool>>, start_x: usize, start_y: usize) -> (bool, Vec<Vec<bool>>) {
    let mut grid = grid_input;
    if !grid[start_y][start_x] { return (false, grid) }
    let mut queue = std::collections::VecDeque::new();
    queue.push_back((start_x, start_y));

    while !queue.is_empty() {
        let (x, y) = queue.pop_front().unwrap();

        if x >= grid.len() || y >= grid[0].len() || grid[y][x] == false {
            continue;
        }

        grid[y][x] = false;

        let max_x = grid[0].len();
        let max_y = grid.len();
        if x > 0 { queue.push_back((x - 1, y)) }
        if x < max_x-1 { queue.push_back((x + 1, y)) }
        if y > 0 { queue.push_back((x, y - 1)) }
        if y < max_y - 1 { queue.push_back((x, y + 1)) }
        
        if x > 0 && y > 0 { queue.push_back((x-1, y-1)) }
        if x > 0 && y < max_y { queue.push_back((x-1, y+1)) }
        if x < max_x && y > 0 { queue.push_back((x+1, y-1)) }
        if x < max_x && y < max_y { queue.push_back((x+1, y+1)) }
    }

    (true, grid)
}

fn main() {
    let mut kart: Vec<Vec<bool>> = std::fs::read_to_string("src/kart.txt")
        .expect("errr")
        .trim()
        .lines()
        .map(|l| l.chars().map(|c| c=='X').collect())
        .collect();
    // println!("{:?}", kart);
    println!("{}, {}", kart.len(), kart[0].len());
    // Loop through every x and y position in the grid, and run flood_fill function.
    
    let mut count: usize = 0;
    for y in 0..kart.len() {
        for x in 0..kart[0].len() {
            let (is_island, new_grid) = flood_fill(kart.clone(), x, y);
            if is_island {
                count += 1;
                kart = new_grid;
            }
        }
    }
    println!("{}", count);
}
