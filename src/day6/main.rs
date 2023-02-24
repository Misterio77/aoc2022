use anyhow::Result;
use std::{
    collections::HashSet,
    io::{self, BufRead},
};

#[derive(Debug)]
pub enum Error {}

fn find_mark(input: &str, mark_size: usize) -> Option<usize> {
    let chars: Vec<char> = input.chars().collect();
    for (index, window) in chars.windows(mark_size).enumerate() {
        let uniques = window.iter().collect::<HashSet<_>>();
        if uniques.len() == window.len() {
            return Some(index + mark_size);
        }
    }
    return None;
}

fn main() -> Result<()> {
    let stdin = io::stdin().lock();
    let lines = stdin.lines();

    for line in lines {
        let line = line?;
        if let Some(pos) = find_mark(&line, 4) {
            println!("Part 1: {pos}");
        }
        if let Some(pos) = find_mark(&line, 14) {
            println!("Part 2: {pos}");
        }
    }

    Ok(())
}
