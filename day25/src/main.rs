use std::{
    cmp::{max, min},
    fmt::{self, Display, Formatter},
    ops::{Add, Sub},
    str::FromStr,
};

use anyhow::{anyhow, Error};

#[derive(Debug, PartialEq, Eq)]
pub struct Snafu(i32);

impl Add for Snafu {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self(Add::add(self.0, other.0))
    }
}
impl Sub for Snafu {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        Self(Sub::sub(self.0, other.0))
    }
}

impl Snafu {
    pub fn new(input: i32) -> Snafu {
        Snafu(input)
    }
}

impl FromStr for Snafu {
    type Err = Error;
    fn from_str(s: &str) -> Result<Snafu, Error> {
        let mut total = 0;

        for (index, character) in s.chars().rev().enumerate() {
            let base = 5_i32.pow(index.try_into()?);
            match character {
                '2' => total += 2 * base,
                '=' => total -= 2 * base,
                '1' => total += base,
                '-' => total -= base,
                _ => {}
            }
        }
        Ok(Snafu(total))
    }
}

impl Display for Snafu {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        // Get amount of digits it will have
        // Pretty much just solving this for x:
        // y / 5^x = 2
        let length = ((self.0 as f32).abs().ln() / 5.0_f32.ln()) as i32;
        dbg!(length);

        let mut string = String::with_capacity(length.try_into().unwrap());
        let mut remainder = self.0;
        dbg!(remainder);
        for power in (0..=length).rev() {
            let digit_value = 5.0_f32.powi(power) as i32;
            dbg!(digit_value);
            let number = remainder as f32 / digit_value as f32;
            let number = max(-2, min(2, number.round() as i32));
            dbg!(number);
            let character = match number {
                -2 => '=',
                -1 => '-',
                0 => '0',
                1 => '1',
                2 => '2',
                _ => unreachable!("This shouldn't happen"),
            };
            string.push(character);

            remainder -= digit_value * number;
            dbg!(remainder);
        }

        if remainder != 0 {
            anyhow!("Couldn't get a SNAFU representation for {}, got a remainder of {}", self.0, remainder);
        }
        write!(f, "{}", string)
    }
}

fn main() -> Result<(), Error> {
    let snafu1 = Snafu::new(2022);
    let snafu2 = dbg!(snafu1.to_string()).parse()?;
    assert_eq!(snafu1, snafu2);
    Ok(())
}
