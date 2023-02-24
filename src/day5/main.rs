use std::{
    io::{self, BufRead},
    str::FromStr,
};

#[derive(Debug, Clone)]
pub enum Error {
    StackNotFound,
    StackEmpty,
    Parsing,
}
impl From<std::num::ParseIntError> for Error {
    fn from(_: std::num::ParseIntError) -> Error {
        Error::Parsing
    }
}
impl From<std::io::Error> for Error {
    fn from(_: std::io::Error) -> Error {
        Error::Parsing
    }
}

pub struct Move {
    qty: usize,
    from: usize,
    to: usize,
}

impl FromStr for Move {
    type Err = Error;
    fn from_str(s: &str) -> Result<Move, Error> {
        let mut split = s.split_whitespace().skip(1).step_by(2);
        let qty = split.next().ok_or(Error::Parsing)?.parse()?;
        let from = split.next().ok_or(Error::Parsing)?.parse()?;
        let to = split.next().ok_or(Error::Parsing)?.parse()?;
        Ok(Move { qty, from, to })
    }
}

#[derive(Debug)]
pub struct Ship(Vec<Vec<char>>);

impl Ship {
    fn push_boxes(&mut self, stack_no: usize, boxes: &[char]) -> Result<(), Error> {
        self.0
            .get_mut(stack_no - 1)
            .ok_or(Error::StackNotFound)?
            .append(&mut boxes.into());
        Ok(())
    }
    fn pop_box(&mut self, stack_no: usize) -> Result<char, Error> {
        self.0
            .get_mut(stack_no - 1)
            .ok_or(Error::StackNotFound)?
            .pop()
            .ok_or(Error::StackEmpty)
    }

    pub fn get_tops(&self) -> Result<String, Error> {
        let stacks = &self.0;
        let mut tops = Vec::with_capacity(stacks.len());
        for stack in stacks.iter() {
            if let Some(top) = stack.last() {
                tops.push(*top);
            }
        }
        Ok(tops.into_iter().collect())
    }
    pub fn move_box(&mut self, mov: Move, preserve_order: bool) -> Result<(), Error> {
        let mut boxes = Vec::with_capacity(mov.qty);
        for _ in 0..mov.qty {
            boxes.push(self.pop_box(mov.from)?);
        }
        if preserve_order {
            boxes = boxes.into_iter().rev().collect();
        }
        self.push_boxes(mov.to, &boxes)?;
        Ok(())
    }
    pub fn run_moves(&mut self, input: &[&str], preserve_order: bool) -> Result<(), Error> {
        for line in input.iter() {
            self.move_box(line.parse()?, preserve_order)?;
        }
        Ok(())
    }
    pub fn with_boxes(input: &[&str]) -> Result<Ship, Error> {
        let mut lines = input.iter().rev().skip(1);

        // Get how many stacks
        let stack_count = lines
            .next()
            .ok_or(Error::Parsing)?
            .split_whitespace()
            .rev()
            .next()
            .ok_or(Error::Parsing)?
            .parse()?;
        // Get how many boxes per stack
        let stack_capacity = lines.len();

        // Allocate and initialize stacks
        let mut stacks: Vec<Vec<char>> = Vec::with_capacity(stack_count);
        for _ in 0..stack_count {
            stacks.push(Vec::with_capacity(stack_capacity));
        }

        // Iterate over box input lines
        for line in lines {
            // Iterate in steps of 4 (to skip "] [")
            let boxes = line.chars().skip(1).step_by(4);
            for (stack_no, current_box) in boxes.enumerate() {
                if !current_box.is_whitespace() {
                    // Add box to stack
                    stacks
                        .get_mut(stack_no)
                        .ok_or(Error::Parsing)?
                        .push(current_box);
                }
            }
        }

        Ok(Ship(stacks))
    }
}

fn main() -> Result<(), Error> {
    let stdin = io::stdin().lock();
    let lines = stdin.lines().into_iter().collect::<Result<Vec<_>, _>>()?;
    let lines = lines.iter().map(|x| x as &str);
    let (moves, boxes): (Vec<&str>, Vec<&str>) = lines.partition(|x| x.starts_with("move"));

    // Part 1
    let mut ship = Ship::with_boxes(&boxes)?;
    ship.run_moves(&moves, false)?;
    println!("Part 1: {}", ship.get_tops()?);

    // Part 2
    let mut ship = Ship::with_boxes(&boxes)?;
    ship.run_moves(&moves, true)?;
    println!("Part 2: {}", ship.get_tops()?);
    Ok(())
}
