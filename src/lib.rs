use std::mem::take;
use std::collections::HashMap;

#[derive(Debug)]
#[derive(Clone)]
pub enum Op {
    Init(usize, usize, u8),
    Propagate(usize, usize, u8, String),
    UpdateCandidates(),
    Decide(usize, usize, u8),
    Conflict(usize, usize, u8),
}

#[derive(Debug)]
pub struct Board {
    cells: [[u8; 9]; 9],
    candidates: [[u16; 9]; 9],
    ops: Vec<Op>,
}

impl Board {
    pub fn new() -> Board {
        Board {
            cells: [[0; 9]; 9],
            candidates: [[Self::full_digit_bits(); 9]; 9],
            ops: Vec::new(),
        }
    }

    // empty line or comment line
    fn is_empty_line(s: &str) -> bool {
        s.chars().all(|c| c.is_whitespace()) || s.starts_with("#")
    }

    // is space, underscore or dot
    fn is_empty_char(c: char) -> bool {
        c.is_whitespace() || c == '_' || c == '.'
    }

    // e.g.
    //  # 0, _ or . for empty cell and space for separation
    //  530 070 000
    //  6__ 195 ___
    //  .98 ... ...
    //  # empty line or comment line
    pub fn parse(s: &str) -> Result<Board, String> {
        let mut board = Board::new();
        for (y, line) in s.lines().filter(|line| !Self::is_empty_line(line) ).enumerate() {
            for (x, c) in line.chars().filter(|c| !Self::is_empty_char(*c) ).enumerate() {
                if c.is_digit(10) {
                    let d = c.to_digit(10).unwrap() as u8;
                    if d == 0 {
                        continue;
                    }
                    board.apply(Op::Init(x, y, d))?;
                } else {
                    return Err(format!("Invalid character '{}' at ({}, {})", c, x, y));
                }
            }
        }
        Ok(board)
    }

    pub fn solve(&mut self) -> Result<(), String> {
        while !self.is_solved() {
            // propagate ot update candidates, then decide
            let op = self.propagate().unwrap_or_else(||
                if let Some(Op::UpdateCandidates()) = self.ops.last() {
                    self.decide()
                } else {
                    // update candidates is little bit expensive, so do it only at no progress
                    Op::UpdateCandidates()
                }
            );
            // backtracking if conflict
            self.apply(op).or_else(|_| self.backtrack())?;
        }
        Ok(())
    }

    pub fn is_solved(&self) -> bool {
        for y in 0..9 {
            for x in 0..9 {
                if self.cells[y][x] == 0 {
                    return false;
                }
            }
        }
        true
    }

    pub fn show(&self) {
        for y in 0..9 {
            for x in 0..9 {
                if self.cells[y][x] == 0 {
                    print!("_");
                } else {
                    print!("{}", self.cells[y][x]);
                }
                if x == 2 || x == 5 {
                    print!(" ");
                }
            }
            println!();
            if y == 2 || y == 5 {
                println!();
            }
        }
    }

    pub fn ops(&self) -> Vec<Op> {
        self.ops.clone()
    }

    //
    // operations
    //

    fn propagate(&mut self) -> Option<Op> {
        // Simple propagation
        for y in 0..9 {
            for x in 0..9 {
                if self.cells[y][x] != 0 {
                    continue;
                }
                if let [d] = self.candidates(x, y).as_slice() {
                    return Some(Op::Propagate(x, y, *d, "only one candidate".to_string()));
                }
            }
        }

        // Only one cell can have the digit in the group
        for (group, name) in Self::groups() {
            for (d, poses) in self.candidate_positions_in_group(&group) {
                if let [(x, y)] = poses.as_slice() {
                    return Some(Op::Propagate(*x, *y, d, format!("only one candidate in {}", name)));
                }
            }
        }
        None
    }

    fn decide(&self) -> Op {
        let mut dx = 0;
        let mut dy = 0;
        let mut dd = 0;
        let mut min = 10;
        for y in 0..9 {
            for x in 0..9 {
                if self.cells[y][x] != 0 {
                    continue;
                }
                let c = Self::condidate_size(self.candidates[y][x]);
                if c < min {
                    min = c;
                    dx = x;
                    dy = y;
                    dd = Self::pick_digit(self.candidates[y][x]);
                }
            }
        }
        Op::Decide(dx, dy, dd)
    }

    fn backtrack(&mut self) -> Result<(), String> {
        println!("backtrack");
        println!("{:?}", self.ops);
        while let Some(op) = self.ops.pop() {
            println!("{:?}", op);
            match op {
                Op::Init(_, _, _) => { self.ops.push(op); return Err("No more decision to backtrack".to_string()); },
                Op::Propagate(_,_,_,_) => continue,
                Op::UpdateCandidates() => continue,
                Op::Decide(x, y, d) => {
                    // Since undoing candidates is difficult, reconstruct the board from scratch
                    self.reconstruct();
                    self.apply(Op::Conflict(x, y, d))?;
                    return Ok(());
                },
                Op::Conflict(_, _, _) => continue,
            }
        }
        Err("No more decision to backtrack".to_string())
    }

    fn apply(&mut self, op: Op) -> Result<(), String> {
        match op {
            Op::Init(x, y, d) => {
                self.set_digit(x, y, d)?;
            },
            Op::Propagate(x, y, d, _) => {
                self.set_digit(x, y, d)?;
            },
            Op::UpdateCandidates() => {
                self.update_candidates()?;
            },
            Op::Decide(x, y, d) => {
                self.set_digit(x, y, d)?;
            },
            Op::Conflict(x, y, d) => {
                self.exclude_candidates(x, y, vec!(d))?;
            },
        }
        self.ops.push(op);
        Ok(())
    }

    fn groups() -> Vec<(Vec<(usize, usize)>, String)> {
        let mut groups = Vec::new();
        // horizontal lines
        for y in 0..9 {
            let mut group = Vec::new();
            for x in 0..9 {
                group.push((x, y));
            }
            groups.push((group, format!("horizontal line {}", y)));
        }

        // vertical lines
        for x in 0..9 {
            let mut group = Vec::new();
            for y in 0..9 {
                group.push((x, y));
            }
            groups.push((group, format!("vertical line {}", x)));
        }

        // blocks
        for y0 in 0..3 {
            for x0 in 0..3 {
                let mut group = Vec::new();
                for y in 0..3 {
                    for x in 0..3 {
                        group.push((x0 * 3 + x, y0 * 3 + y));
                    }
                }
                groups.push((group, format!("block ({}, {})", x0, y0)));
            }
        }

        groups
    }

    fn groups_for(x: usize, y: usize) -> Vec<(Vec<(usize, usize)>, String)> {
        let mut groups = Vec::new();
        for (group, name) in Self::groups() {
            if group.iter().any(|(x0, y0)| *x0 == x && *y0 == y) {
                groups.push((group, name));
            }
        }
        groups
    }

    fn update_candidates(&mut self) -> Result<(), String> {
        // Exact N cells have same N candidates in the group
        for (group, name) in Self::groups() {
            let poses = self.candidate_positions_in_group(&group);  // digit -> [(x, y)]
            let mut inverse_map = HashMap::new();  // Vec<(x,y)> -> Vec<digit>
            for (d, ps) in poses.iter() {
                inverse_map.entry(ps).or_insert(Vec::new()).push(*d);
            }
            for (ps, ds) in inverse_map.iter() {
                if ds.len() != ps.len() {
                    continue;
                }
                // ds(num of candidates) == ps(num of free cells in the group)
                // these cells must have the same candidates, and exclude other candidates
                for (x, y) in group.iter() {
                    if self.cells[*y][*x] != 0 {
                        continue;
                    }
                    if ps.contains(&(*x, *y)) {
                        // set candidates
                        self.set_candidates(*x, *y, ds.clone());
                    } else {
                        // exclude candidates
                        self.exclude_candidates(*x, *y, ds.clone())?;
                    }
                }
            }
        }

        // Only N candidates can be in N cells in the group
        for (group, name) in Self::groups() {
            let mut inverse_map = HashMap::new();  // Vec<u8> -> Vec<(x,y)>
            for ((x, y), ds) in self.candidates_by_potision_in_group(&group) {
                inverse_map.entry(ds).or_insert(Vec::new()).push((x, y));
            }
            for (ds, ps) in inverse_map.iter() {
                if ds.len() != ps.len() {
                    continue;
                }
                for (x, y) in group.iter() {
                    if self.cells[*y][*x] != 0 {
                        continue;
                    }
                    if ps.contains(&(*x, *y)) {
                        // set candidates
                        self.set_candidates(*x, *y, ds.clone());
                    } else {
                        // exclude candidates
                        self.exclude_candidates(*x, *y, ds.clone())?;
                    }
                }
            }
        }

        // group intersection
        for (group1, name1) in Self::groups() {
            let digit_poses = self.candidate_positions_in_group(&group1);
            for (group2, name2) in Self::groups() {
                if group1 == group2 {
                    continue;
                }
                for (d, poses) in digit_poses.iter() {
                    if !poses.iter().all(|(x, y)| group2.contains(&(*x, *y))) {
                        continue;
                    }
                    for (x, y) in group2.iter() {
                        if self.cells[*y][*x] != 0 {
                            continue;
                        }
                        if !poses.contains(&(*x, *y)) {
                            // exclude candidates
                            self.exclude_candidates(*x, *y, vec![*d])?;
                        }
                    }
                }
            }
        }

        Ok(())
    }

    //
    // Board operations
    //

    fn set_digit(&mut self, x: usize, y: usize, d: u8) -> Result<(), String> {
        self.cells[y][x] = d;
        self.candidates[y][x] = Self::digit_to_bit(d);

        // exclude candidates in the same group
        for (group, _) in Self::groups_for(x, y) {
            for (x0, y0) in group.iter() {
                if *x0 == x && *y0 == y {
                    continue;
                }
                self.exclude_candidates(*x0, *y0, vec!(d))?;
            }
        }
        Ok(())
    }

    fn candidates(&self, x: usize, y: usize) -> Vec<u8> {
        Self::bits_to_digits(self.candidates[y][x])
    }

    fn set_candidates(&mut self, x: usize, y: usize, ds: Vec<u8>) {
        self.candidates[y][x] = Self::digits_to_bits(ds);
    }

    fn exclude_candidates(&mut self, x: usize, y: usize, ds: Vec<u8>) -> Result<(), String> {
        self.candidates[y][x] &= !Self::digits_to_bits(ds);
        if Self::condidate_size(self.candidates[y][x]) == 0 {
            Err(format!("Conflict at ({}, {})", x, y))
        } else {
            Ok(())
        }
    }

    // (x, y) -> [digit] in the group
    fn candidates_by_potision_in_group(&self, group: &Vec<(usize, usize)>) -> HashMap<(usize, usize), Vec<u8>> {
        let mut poses = HashMap::new();
        for (x, y) in group.iter() {
            if self.cells[*y][*x] != 0 {
                continue;
            }
            poses.insert((*x, *y), self.candidates(*x, *y));
        }
        poses
    }

    // digit -> [(x, y)] in the group
    fn candidate_positions_in_group(&self, group: &Vec<(usize, usize)>) -> HashMap<u8, Vec<(usize, usize)>> {
        let mut poses = HashMap::new();
        for (x, y) in group.iter() {
            if self.cells[*y][*x] != 0 {
                continue;
            }
            for d in Self::bits_to_digits(self.candidates[*y][*x]) {
                poses.entry(d).or_insert(Vec::new()).push((*x, *y));
            }
        }
        poses
    }

    fn reconstruct(&mut self) {
        println!("reconstruct");
        let ops = take(&mut self.ops);
        *self = Board::new();
        for op in ops {
            self.apply(op).unwrap();
        }
    }

    //
    // bit operations
    //

    fn full_digit_bits() -> u16 {
        0b111111111
    }
    fn digit_to_bit(d: u8) -> u16 {
        1 << (d - 1)
    }
    fn bit_to_digit(b: u16) -> u8 {
        b.trailing_zeros() as u8 + 1
    }
    fn pick_digit(b: u16) -> u8 {
        Self::bit_to_digit(b)
    }
    fn condidate_size(b: u16) -> u8 {
        b.count_ones() as u8
    }
    fn bits_to_digits(mut b: u16) -> Vec<u8> {
        let mut digits = Vec::new();
        for d in 1..=9 {
            if b & 1 != 0 {
                digits.push(d);
            }
            b >>= 1;
        }
        digits
    }
    fn digits_to_bits(ds: Vec<u8>) -> u16 {
        let mut bits = 0;
        for d in ds.iter() {
            bits |= Self::digit_to_bit(*d);
        }
        bits
    }

}
