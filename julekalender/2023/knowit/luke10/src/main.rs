fn flood_fill(grid_input: [[bool; 8]; 8], start_x: usize, start_y: usize) -> (usize, [[bool; 8]; 8]) {
    let mut grid = grid_input;
    let mut filled_count = 0;
    let mut queue = std::collections::VecDeque::new();
    queue.push_back((start_x, start_y));

    while !queue.is_empty() {
        let (x, y) = queue.pop_front().unwrap();

        if x >= grid.len() || y >= grid[0].len() || grid[y][x] == false {
            continue;
        }

        filled_count += 1;
        grid[y][x] = false;

        if x > 0 { queue.push_back((x - 1, y)) }
        queue.push_back((x + 1, y));
        if y > 0 { queue.push_back((x, y - 1)) }
        queue.push_back((x, y + 1));
    }

    (filled_count, grid)
}

fn can_be_cut(grid: [[bool; 8]; 8]) -> bool {
    // This is missing a check to find out if the cut has split it into only two pieces
    let mut total_pieces: usize = 0;
    for row in 0..grid.len() {
        for col in 0..grid[row].len() {
            if grid[row][col] {
                total_pieces = flood_fill(grid, col, row).0;
            }
        }
    }

    // Test vertical cuts
    for col in 1..grid[0].len()-1 { // The number represents the top left corner
                                 // It goes with a padding of one to ensure the cut is valid
        for start_y in 0..grid.len() {
            for end_y in 0..grid.len() {
                if start_y == end_y { continue }
                let mut grid_left = grid; // The left side is left
                let mut grid_right = grid; // The right side is left
                let mut y_to_test_left: usize = start_y;
                let mut y_to_test_right: usize = start_y;
                let mut possible_right = false;
                let mut possible_left = false;
                for y in start_y..=end_y {
                    grid_left[y][col] = false;
                    grid_right[y][col-1] = false;
                    if grid_left[y][col-1] { y_to_test_left = y; possible_left = true; }
                    if grid_right[y][col] { y_to_test_right = y; possible_right = true; }
                }
                if !possible_right || !possible_left { continue }

                let (left_size, left_grid) = flood_fill(grid_left, col-1, y_to_test_left);
                let (right_size, right_grid) = flood_fill(grid_right, col, y_to_test_right);
                // Add a check to see if there are any pieces left that were not removed
                // println!("{:?}", left_grid);
                // println!("{:?}", right_grid);
                // println!("{:?}", grid_right);
                let mut new_grid = left_grid;
                for row in 0..right_grid.len() {
                    for col in 0..right_grid[0].len() {
                        if !right_grid[row][col] {
                            new_grid[row][col] = false;
                        }
                    }
                }
                // println!("{:?}", left_grid);
                if new_grid != [[false; 8]; 8] { continue }
                if left_size == right_size && left_size + right_size == total_pieces { 
                    // println!("Left grid (both removed) {:?}", left_grid);
                    // println!("Right grid {:?}", right_grid);
                    // println!("Left initial {:?}", grid_left);
                    // println!("Right initial {:?}", grid_right);
                    // println!("X {}, Y1 {}, Y2 {}", col, start_y, end_y);
                    return true;
                }

                // println!("Left Grid: {:?}", grid_left);
                // println!("Right Grid: {:?}", grid_right);
                // println!("Left y: {}, Right y: {}, Count left: {}, Count right: {}, Col: {}",
                //          y_to_test_left, y_to_test_right, left_size, right_size, col);
                // return false
            }
        }
    }

    // Testing horizontal cuts will be basically the same, but with some values swapped
    for row in 1..grid.len()-1 {
        for start_x in 0..grid[0].len() {
            for end_x in 0..grid[0].len() {
                if start_x == end_x { continue }
                let mut grid_top = grid;
                let mut grid_bottom = grid;
                let mut x_to_test_top: usize = start_x;
                let mut x_to_test_bottom: usize = start_x;
                let mut possible_top = false;
                let mut possible_bottom = false;
                for x in start_x..=end_x {
                    grid_top[row][x] = false;
                    grid_bottom[row-1][x] = false;
                    if grid_top[row-1][x] { x_to_test_top = x; possible_top = true; }
                    if grid_bottom[row][x] { x_to_test_bottom = x; possible_bottom = true; }
                }
                if !possible_top || !possible_bottom { continue }

                let (top_size, top_grid) = flood_fill(grid_top, x_to_test_top, row-1);
                let (bottom_size, bottom_grid) = flood_fill(grid_bottom, x_to_test_bottom, row);
                let mut new_grid = top_grid;
                for row in 0..bottom_grid.len() {
                    for col in 0..bottom_grid[0].len() {
                        if !bottom_grid[row][col] {
                            new_grid[row][col] = false;
                        }
                    }
                }
                if new_grid != [[false; 8]; 8] { continue }
                if top_size == bottom_size && top_size + bottom_size == total_pieces {
                    // println!("top initial {:?}", grid_top);
                    // println!("top grid {:?}", top_grid);
                    // println!("bottom initial {:?}", grid_bottom);
                    // println!("bottom grid {:?}", bottom_grid);
                    // println!("both removed {:?}", new_grid);
                    // println!("original {:?}", grid);
                    // println!("hmm {}, {}", grid_top==grid, grid_bottom==grid);
                    // println!("Y {}, X1 {}, X2 {}", row, start_x, end_x);
                    return true;
                }

                // println!("Top Grid: {:?}", grid_top);
                // println!("Bottom Grid: {:?}", grid_bottom);
                // println!("Top x: {}, Bottom x: {}, Count top: {}, Count bottom: {}, Row: {}",
                //          x_to_test_top, x_to_test_bottom, top_size, bottom_size, row);
                // return false;
            }
        }
    }

    false
}




fn main() {
    let input: Vec<[[bool; 8]; 8]> = std::fs::read_to_string("src/sjokkis.txt")
        .expect("hmm")
        .trim()
        .lines()
        .map(|line| {
            let mut array = [[false; 8]; 8];
            for (i, c) in line.chars().enumerate() {
                array[i / 8][i % 8] = c == '1';
            }
            array
        })
        .collect();
    // println!("{:?}", input);

    let mut total_possible: usize = 0;
    for plate in input {
        if can_be_cut(plate) { total_possible += 1 }
    }
    println!("{}", total_possible); // Er visst en mindre enn riktig svar
    
    // let example: [[bool; 8]; 8] = [
    //     [true , true , true , false, true , false, true , false],
    //     [true , true , true , true , true , true , true , false],
    //     [true , true , false, false, true , false, false, false],
    //     [true , false, true , true , true , false, false, false],
    //     [true , true , true , true , false, false, false, false],
    //     [true , false, false, true , true , false, false, false],
    //     [false, false, false, true , true , false, false, false],
    //     [false, false, false, false, false, false, false, false],
    // ];
    // let can_cut = can_be_cut(example);
    // println!("{}", can_cut);
}
