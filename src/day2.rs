use std::error;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::vec::Vec;

fn get_lines(filename: &str) -> Result<std::io::Lines<io::BufReader<std::fs::File>>> {
    let path = Path::new(&filename);
    let file = File::open(path)?;
    Ok(io::BufReader::new(file).lines())
}

#[derive(Debug)]
struct Password {
    pwd: std::string::String,
    low: i32,
    high: i32,
    c: char,
}

// Change the alias to `Box<error::Error>`.
type Result<T> = std::result::Result<T, Box<dyn error::Error>>;

fn parse_password(line: &str) -> Result<Password> {
    // 1-9 a: abcdefg
    let mut iter = line.split_whitespace();

    let range: Vec<_> = iter
        .next()
        .ok_or("failed to parse range")?
        .to_string()
        .split('-')
        .map(|s| s.parse::<i32>())
        .collect();
    let (low, high) = (range[0].clone()?, range[1].clone()?);
    let mid = iter.next().ok_or("failed to parse mid")?;
    let pwd = iter.next().ok_or("failed to parse pwd")?;

    let p = Password {
        low,
        high,
        c: mid.chars().nth(0).ok_or("mid is empty")?,
        pwd: pwd.to_string(),
    };
    Ok(p)
}

fn valid_count(line: &str) -> Result<bool> {
    let p = parse_password(line)?;
    let count = p.pwd.chars().filter(|x| x == &p.c).count() as i32;
    Ok(p.low <= count && count <= p.high)
}

type BoolFn = fn(&str) -> Result<bool>;

fn num_valid(lines: std::io::Lines<io::BufReader<std::fs::File>>, is_valid: BoolFn) -> Result<i32> {
    let results: Result<Vec<_>> = lines.map(|line| is_valid(&line?)).collect();
    Ok(results?.iter().filter(|x| **x).count() as i32)
}

fn valid_position(line: &str) -> Result<bool> {
    let p = parse_password(line)?;
    let x = p.pwd.as_bytes()[(p.low - 1) as usize] as char;
    let y = p.pwd.as_bytes()[(p.high - 1) as usize] as char;

    Ok(1 == (((p.c == x) as i32) + ((p.c == y) as i32)))
}

fn run_test(filename: &str, is_valid: BoolFn) -> Result<()> {
    let example_lines = get_lines(filename)?;
    println!("{}", num_valid(example_lines, is_valid)?);
    Ok(())
}

fn main() {
    for (filename, is_valid) in vec![
        ("example.input", valid_count as BoolFn),
        ("problem.input", valid_count as BoolFn),
        ("problem.input", valid_position as BoolFn),
    ] {
        if let Err(e) = run_test(filename, is_valid) {
            println!("{:?}", e);
        }
    }
}
