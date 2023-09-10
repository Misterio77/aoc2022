use anyhow::{anyhow, Context, Result};
use std::collections::HashMap as Map;

#[derive(Debug, Clone)]
pub enum File {
    Regular(u64),
    Directory(Map<String, File>),
}

impl File {
    pub fn new_reg(size: u64) -> Self {
        File::Regular(size)
    }
    pub fn new_dir() -> Self {
        File::Directory(Map::new())
    }

    pub fn get_total_size(&self) -> Option<u64> {
        match &self {
            File::Regular(size) => Some(*size),
            File::Directory(children) => children.values().map(File::get_total_size).sum(),
        }
    }

    pub fn append(&mut self, name: &str, value: File) -> Result<()> {
        match self {
            File::Directory(children) => {
                children.insert(name.into(), value);
                Ok(())
            }
            _ => Err(anyhow!("You can't fill a regular file")),
        }
    }
    pub fn traverse(&mut self, name: &str) -> Result<&mut Self> {
        match self {
            File::Directory(children) => children.get_mut(name).context("Couldn't find file"),
            _ => Err(anyhow!("Can't traverse a regular file")),
        }
    }
    pub fn from_line(input: &str) -> Result<(String, Self)> {
        let mut tokens = input.split_whitespace();
        let first = tokens.next().context("Invalid file listing")?;
        let name = tokens.next().context("Invalid file listing")?;

        if first == "dir" {
            Ok((name.into(), File::Directory(Map::new())))
        } else {
            let size = first.parse()?;
            Ok((name.into(), File::Regular(size)))
        }
    }
}

fn main() -> Result<()> {
    let mut root = File::Directory(Map::new());

    {
        let cur = &mut root;
        cur.append("a", File::new_dir())?;
        cur.append("b.txt", File::new_reg(14848514))?;
        cur.append("c.dat", File::new_reg(8504156))?;
        cur.append("d", File::new_dir())?;
        {
            let cur = cur.traverse("a")?;
            cur.append("e", File::new_dir())?;
            cur.append("f", File::new_reg(29116))?;
            cur.append("g", File::new_reg(2557))?;
            cur.append("h.lst", File::new_reg(62596))?;
            {
                let cur = cur.traverse("e")?;
                cur.append("i", File::new_reg(584))?;
            }
        }
        let cur = cur.traverse("d")?;
        cur.append("j", File::new_reg(4060174))?;
        cur.append("d.log", File::new_reg(8033020))?;
        cur.append("d.ext", File::new_reg(5626152))?;
        cur.append("k", File::new_reg(7214296))?;
    }

    println!("{:?}", root);
    Ok(())
}
