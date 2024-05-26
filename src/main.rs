fn main() -> Result<(), String> {
    let args: Vec<String> = std::env::args().collect();
    for arg in args.iter().skip(1) {
        println!("=== {} ===", arg);
        let s = std::fs::read_to_string(arg).unwrap();
        let mut board = sudoku_solver::Board::parse(&s)?;
        board.solve().or_else(|e| {
            println!("{:?}", board);
            println!("{}", e);
            Err(e)
        })?;
        board.show();
        println!();
    }
    Ok(())
}
