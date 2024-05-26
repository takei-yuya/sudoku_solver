use std::mem::take;
use std::collections::HashMap;

#[derive(Debug)]
#[derive(Clone)]
pub enum Op {
    Init(usize, usize, u8),
    Propagate(usize, usize, u8, String),
    Decide(usize, usize, u8),
    Conflict(usize, usize, u8),
}

#[derive(Debug)]
pub struct Board {
    cells: [[u8; 9]; 9],
    candidates: [[u64; 9]; 9],
    ops: Vec<Op>,
}

impl Board {
    fn full_digit_bits() -> u64 {
        // To count the num of each digit by +, use 4 bits for each digit
        //   9   8   7   6   5   4   3   2   1
        0b000100010001000100010001000100010001
    }
    fn digit_to_bit(d: u8) -> u64 {
        1 << (d - 1) * 4
    }
    fn bit_to_digit(b: u64) -> u8 {
        b.trailing_zeros() as u8 / 4 + 1
    }
    fn pick_digit(b: u64) -> u8 {
        Self::bit_to_digit(b)
    }
    fn condidate_size(b: u64) -> u8 {
        b.count_ones() as u8
    }
    fn bits_to_digits(mut b: u64) -> Vec<u8> {
        let mut digits = Vec::new();
        for d in 0..9 {
            if b & 0b1111 != 0 {
                digits.push(d + 1);
            }
            b >>= 4;
        }
        digits
    }

    pub fn new() -> Board {
        Board {
            cells: [[0; 9]; 9],
            candidates: [[Self::full_digit_bits(); 9]; 9],
            ops: Vec::new(),
        }
    }

    pub fn parse(s: &str) -> Result<Board, String> {
        let mut board = Board::new();
        for (y, line) in s.lines().filter(|line| line.len() > 0).enumerate() {
            for (x, c) in line.chars().filter(|c| !c.is_whitespace()).enumerate() {
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
            match self.propagate() {
                Ok(Some(op)) => self.apply(op)?,
                Ok(None) => {
                    let op = self.decide();
                    self.apply(op)?;
                },
                Err(msg) => {
                    println!("Conflict: {}", msg);
                    self.backtrack()?;
                },
            }
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

    fn apply(&mut self, op: Op) -> Result<(), String> {
        // println!("{:?}", op);
        match op {
            Op::Init(x, y, d) => {
                self.set(x, y, d)?;
            },
            Op::Propagate(x, y, d, _) => {
                self.set(x, y, d)?;
            },
            Op::Decide(x, y, d) => {
                self.set(x, y, d)?;
            },
            Op::Conflict(x, y, d) => {
                self.exclude(x, y, d)?;
            },
        }
        self.ops.push(op);
        Ok(())
    }

    fn set(&mut self, x: usize, y: usize, d: u8) -> Result<(), String> {
        self.cells[y][x] = d;
        self.candidates[y][x] = Self::digit_to_bit(d) * 9;
        // line
        for i in 0..9 {
            self.exclude(x, i, d)?;
            self.exclude(i, y, d)?;
        }
        // block
        let x0 = x / 3 * 3;
        let y0 = y / 3 * 3;
        for i in 0..3 {
            for j in 0..3 {
                self.exclude(x0 + i, y0 + j, d)?;
            }
        }
        Ok(())
    }

    // Return true if the candidates are updated
    fn set_candidates(&mut self, x: usize, y: usize, ds: Vec<u8>) -> bool {
        let mut candidates = 0;
        for d in ds.iter() {
            candidates |= Self::digit_to_bit(*d);
        }
        let old = self.candidates[y][x];
        self.candidates[y][x] = candidates;
        self.candidates[y][x] != old
    }

    // Return true if the candidates are updated
    fn unset_candidates(&mut self, x: usize, y: usize, ds: Vec<u8>) -> Result<bool, String> {
        let mut candidates = 0;
        for d in ds.iter() {
            candidates |= Self::digit_to_bit(*d);
        }
        let old = self.candidates[y][x];
        self.candidates[y][x] &= !candidates;
        if self.candidates[y][x] == 0 {
            println!("Conflict at ({}, {})", x, y);
            Err(format!("Conflict at ({}, {})", x, y))
        } else {
            Ok(self.candidates[y][x] != old)
        }
    }

    // TODO: Refactoring
    fn exclude(&mut self, x: usize, y: usize, d: u8) -> Result<(), String> {
        if self.cells[y][x] != 0 {
            return Ok(());
        }
        self.candidates[y][x] &= !Self::digit_to_bit(d);
        if Self::condidate_size(self.candidates[y][x]) == 0 {
            println!("Conflict at ({}, {})", x, y);
            Err(format!("Conflict at ({}, {})", x, y))
        } else {
            Ok(())
        }
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

    fn candidates(&self, x: usize, y: usize) -> Vec<u8> {
        Self::bits_to_digits(self.candidates[y][x])
    }

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

    // TODO: Result<Option<..>, ..> seems to be a bad design
    fn propagate(&mut self) -> Result<Option<Op>, String> {
        // Simple propagation
        for y in 0..9 {
            for x in 0..9 {
                if self.cells[y][x] != 0 {
                    continue;
                }
                if let [d] = self.candidates(x, y).as_slice() {
                    return Ok(Some(Op::Propagate(x, y, *d, "only one candidate".to_string())));
                }
            }
        }

        // Only one cell can have the digit in the group
        for (group, name) in Self::groups() {
            for (d, poses) in self.candidate_positions_in_group(&group) {
                if let [(x, y)] = poses.as_slice() {
                    return Ok(Some(Op::Propagate(*x, *y, d, format!("only one candidate in {}", name))));
                }
            }
        }

        // Complex candidates update
        // TODO: Refactoring

        let mut changed = false;
        // Exact N cells have same N candidates in the group
        for (group, name) in Self::groups() {
            let poses = self.candidate_positions_in_group(&group);
            let mut inverse_map = HashMap::new();  // Vec<(x,y)> -> Vec<u8>
            for (d, ps) in poses.iter() {
                inverse_map.entry(ps).or_insert(Vec::new()).push(*d);
            }
            for (ps, ds) in inverse_map.iter() {
                if ds.len() != ps.len() {
                    continue;
                }
                for (x, y) in group.iter() {
                    if self.cells[*y][*x] != 0 {
                        continue;
                    }
                    if ps.contains(&(*x, *y)) {
                        // set candidates
                        println!("set {:?} as candidates at ({}, {}) in {}", ds, x, y, name);
                        changed |= self.set_candidates(*x, *y, ds.clone());
                    } else {
                        // exclude candidates
                        println!("unset {:?} as candidates at ({}, {}) in {}", ds, x, y, name);
                        changed |= self.unset_candidates(*x, *y, ds.clone())?;
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
                        println!("set {:?} as candidates at ({}, {}) in {}", ds, x, y, name);
                        changed |= self.set_candidates(*x, *y, ds.clone());
                    } else {
                        // exclude candidates
                        println!("unset {:?} as candidates at ({}, {}) in {}", ds, x, y, name);
                        changed |= self.unset_candidates(*x, *y, ds.clone())?;
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
                            println!("unset {:?} as candidates at ({}, {}) in {} and {}", d, x, y, name1, name2);
                            changed |= self.unset_candidates(*x, *y, vec![*d])?;
                        }
                    }
                }
            }
        }

        if changed {
            return self.propagate();
        }

        // TODO: More complex propagation
        Ok(None)
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

    fn reconstruct(&mut self) {
        println!("reconstruct");
        let ops = take(&mut self.ops);
        *self = Board::new();
        for op in ops {
            self.apply(op).unwrap();
        }
    }

    fn backtrack(&mut self) -> Result<(), String> {
        println!("backtrack");
        println!("{:?}", self.ops);
        while let Some(op) = self.ops.pop() {
            println!("{:?}", op);
            match op {
                Op::Init(_, _, _) => { self.ops.push(op); return Err("No more decision to backtrack".to_string()); },
                Op::Propagate(_,_,_,_) => continue,
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
}
