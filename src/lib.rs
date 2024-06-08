use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Display;
use std::mem::take;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Digit(u8);

impl Digit {
    fn new(d: u8) -> Digit {
        if d == 0 || d > 9 {
            panic!("Invalid digit {}", d);
        }
        Digit(d)
    }

    fn empty() -> Digit {
        Digit(0)
    }

    fn is_empty(&self) -> bool {
        self.0 == 0
    }

    fn present(&self) -> bool {
        self.0 != 0
    }
}

impl Display for Digit {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl Debug for Digit {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

#[derive(Debug, Clone, Copy)]
struct Candidates(u16);

impl Candidates {
    fn new(c: u16) -> Candidates {
        Candidates(c)
    }

    fn full() -> Candidates {
        Candidates(0b111111111)
    }

    fn from_digit(d: Digit) -> Candidates {
        Candidates(1 << (d.0 - 1))
    }

    fn from_digits(ds: Vec<Digit>) -> Candidates {
        let mut bits = 0;
        for d in ds.iter() {
            bits |= 1 << (d.0 - 1);
        }
        Candidates(bits)
    }

    fn size(&self) -> u8 {
        self.0.count_ones() as u8
    }

    fn pick_digit(&self) -> Digit {
        Digit::new(self.0.trailing_zeros() as u8 + 1)
    }

    fn to_digits(&self) -> Vec<Digit> {
        let mut bits = self.0;
        let mut digits = Vec::new();
        for d in 1..=9 {
            if bits & 1 != 0 {
                digits.push(Digit::new(d));
            }
            bits >>= 1;
        }
        digits
    }

    fn exclude_digits(&self, ds: Vec<Digit>) -> Candidates {
        Candidates::new(self.0 & !Candidates::from_digits(ds).0)
    }
}

#[derive(Debug, Clone)]
pub enum Op {
    Init(usize, usize, Digit),
    Propagate(usize, usize, Digit, String),
    UpdateCandidates(),
    Decide(usize, usize, Digit),
    Conflict(usize, usize, Digit),
}

#[derive(Debug)]
pub struct Board {
    cells: [[Digit; 9]; 9],
    candidates: [[Candidates; 9]; 9],
    ops: Vec<Op>,
}

impl Board {
    pub fn new() -> Board {
        Board {
            cells: [[Digit::empty(); 9]; 9],
            candidates: [[Candidates::full(); 9]; 9],
            ops: Vec::new(),
        }
    }

    // empty line or comment line
    fn is_empty_line(s: &str) -> bool {
        s.chars().all(|c| c.is_whitespace()) || s.starts_with("#")
    }

    // is space
    fn is_empty_char(c: char) -> bool {
        c.is_whitespace()
    }

    // e.g.
    //  # 0, _ or . for empty cell and space for separation
    //  530 070 000
    //  6__ 195 ___
    //  .98 ... ...
    //  # empty line or comment line
    pub fn parse(s: &str) -> Result<Board, String> {
        let mut board = Board::new();
        for (y, line) in s
            .lines()
            .filter(|line| !Self::is_empty_line(line))
            .enumerate()
        {
            if y >= 9 {
                return Err("Too many lines".to_string());
            }
            for (x, c) in line
                .chars()
                .filter(|c| !Self::is_empty_char(*c))
                .enumerate()
            {
                if x >= 9 {
                    return Err("Too many characters in a line".to_string());
                }
                if c == '_' || c == '.' || c == '0' {
                    continue;
                }
                if !c.is_digit(10) {
                    return Err(format!("Invalid character '{}' at ({}, {})", c, x, y));
                }
                let d = c.to_digit(10).unwrap() as u8;
                if d == 0 {
                    continue;
                }
                board.apply(Op::Init(x, y, Digit::new(d)))?;
            }
        }
        Ok(board)
    }

    pub fn solve(&mut self) -> Result<(), String> {
        while !self.is_solved() {
            // propagate ot update candidates, then decide
            let op = self.propagate().unwrap_or_else(|| {
                if let Some(Op::UpdateCandidates()) = self.ops.last() {
                    self.decide()
                } else {
                    // update candidates is little bit expensive, so do it only at no progress
                    Op::UpdateCandidates()
                }
            });
            // backtracking if conflict
            self.apply(op).or_else(|_| self.backtrack())?;
        }
        Ok(())
    }

    pub fn is_solved(&self) -> bool {
        for y in 0..9 {
            for x in 0..9 {
                if self.cells[y][x].is_empty() {
                    return false;
                }
            }
        }
        true
    }

    pub fn show(&self) {
        for y in 0..9 {
            for x in 0..9 {
                if self.cells[y][x].is_empty() {
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
                if self.cells[y][x].present() {
                    continue;
                }
                if let [d] = self.candidates[y][x].to_digits().as_slice() {
                    return Some(Op::Propagate(x, y, *d, "only one candidate".to_string()));
                }
            }
        }

        // Only one cell can have the digit in the group
        for (group, name) in Self::groups() {
            for (d, poses) in self.candidate_positions_in_group(&group) {
                if let [(x, y)] = poses.as_slice() {
                    return Some(Op::Propagate(
                        *x,
                        *y,
                        d,
                        format!("only one candidate in {}", name),
                    ));
                }
            }
        }
        None
    }

    fn decide(&self) -> Op {
        // Find the cell with the smallest number of candidates
        let mut dx = 0;
        let mut dy = 0;
        let mut dd = Digit::empty();
        let mut min = 10;
        for y in 0..9 {
            for x in 0..9 {
                if self.cells[y][x].present() {
                    continue;
                }
                let c = self.candidates[y][x].size();
                if c < min {
                    min = c;
                    dx = x;
                    dy = y;
                    dd = self.candidates[y][x].pick_digit();
                }
            }
        }
        Op::Decide(dx, dy, dd)
    }

    fn backtrack(&mut self) -> Result<(), String> {
        println!("backtrack");
        println!("{:?}", self.ops);
        // rewind to the last decision
        while let Some(op) = self.ops.pop() {
            println!("{:?}", op);
            match op {
                Op::Init(_, _, _) => {
                    self.ops.push(op);
                    return Err("No more decision to backtrack".to_string());
                }
                Op::Propagate(_, _, _, _) => continue,
                Op::UpdateCandidates() => continue,
                Op::Decide(x, y, d) => {
                    // Since undoing candidates is difficult, reconstruct the board from scratch
                    self.reconstruct();
                    self.apply(Op::Conflict(x, y, d))?;
                    return Ok(());
                }
                Op::Conflict(_, _, _) => continue,
            }
        }
        Err("No more decision to backtrack".to_string())
    }

    fn apply(&mut self, op: Op) -> Result<(), String> {
        match op {
            Op::Init(x, y, d) => self.set_digit(x, y, d)?,
            Op::Propagate(x, y, d, _) => self.set_digit(x, y, d)?,
            Op::UpdateCandidates() => self.update_candidates()?,
            Op::Decide(x, y, d) => self.set_digit(x, y, d)?,
            Op::Conflict(x, y, d) => self.exclude_candidates(x, y, vec![d])?,
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
        for (group, _name) in Self::groups() {
            let poses = self.candidate_positions_in_group(&group); // digit -> [(x, y)]
            let mut inverse_map = HashMap::new(); // Vec<(x,y)> -> Vec<digit>
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
                    if self.cells[*y][*x].present() {
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
        for (group, _name) in Self::groups() {
            let mut inverse_map = HashMap::new(); // Vec<Digit> -> Vec<(x,y)>
            for ((x, y), ds) in self.candidates_by_potision_in_group(&group) {
                inverse_map.entry(ds).or_insert(Vec::new()).push((x, y));
            }
            for (ds, ps) in inverse_map.iter() {
                if ds.len() != ps.len() {
                    continue;
                }
                for (x, y) in group.iter() {
                    if self.cells[*y][*x].present() {
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
        for (group1, _name1) in Self::groups() {
            let digit_poses = self.candidate_positions_in_group(&group1);
            for (group2, _name2) in Self::groups() {
                if group1 == group2 {
                    continue;
                }
                for (d, poses) in digit_poses.iter() {
                    if !poses.iter().all(|(x, y)| group2.contains(&(*x, *y))) {
                        continue;
                    }
                    for (x, y) in group2.iter() {
                        if self.cells[*y][*x].present() {
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

    fn set_digit(&mut self, x: usize, y: usize, d: Digit) -> Result<(), String> {
        self.cells[y][x] = d;
        self.candidates[y][x] = Candidates::from_digit(d);

        // exclude candidates in the same group
        for (group, _) in Self::groups_for(x, y) {
            for (x0, y0) in group.iter() {
                if *x0 == x && *y0 == y {
                    continue;
                }
                self.exclude_candidates(*x0, *y0, vec![d])?;
            }
        }
        Ok(())
    }

    fn set_candidates(&mut self, x: usize, y: usize, ds: Vec<Digit>) {
        self.candidates[y][x] = Candidates::from_digits(ds);
    }

    fn exclude_candidates(&mut self, x: usize, y: usize, ds: Vec<Digit>) -> Result<(), String> {
        self.candidates[y][x] = self.candidates[y][x].exclude_digits(ds);
        if self.candidates[y][x].size() == 0 {
            Err(format!("Conflict at ({}, {})", x, y))
        } else {
            Ok(())
        }
    }

    // (x, y) -> [digit] in the group
    fn candidates_by_potision_in_group(
        &self,
        group: &Vec<(usize, usize)>,
    ) -> HashMap<(usize, usize), Vec<Digit>> {
        let mut poses = HashMap::new();
        for (x, y) in group.iter() {
            if self.cells[*y][*x].present() {
                continue;
            }
            poses.insert((*x, *y), self.candidates[*y][*x].to_digits());
        }
        poses
    }

    // digit -> [(x, y)] in the group
    fn candidate_positions_in_group(
        &self,
        group: &Vec<(usize, usize)>,
    ) -> HashMap<Digit, Vec<(usize, usize)>> {
        let mut poses = HashMap::new();
        for (x, y) in group.iter() {
            if self.cells[*y][*x].present() {
                continue;
            }
            for d in self.candidates[*y][*x].to_digits() {
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
}
