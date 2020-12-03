use std::fs::File;
use std::path::Path;
use std::vec::Vec;
use std::io::{self, BufRead};

fn get_lines(filename: &str)  -> std::io::Lines<io::BufReader<std::fs::File>> {
    let path = Path::new(&filename);
    let file = File::open(path).unwrap();
    io::BufReader::new(file).lines()
}

#[derive(Debug)]
struct Password {
    pwd: std::string::String,
    low: i32,
    high: i32,
    c: char
}

fn parse_password(line: &str) -> Password {
    // 1-9 a: abcdefg
    let mut iter = line.split_whitespace();

    let range: Vec<i32> = iter.next().unwrap().to_string().split('-').map(|s| s.parse::<i32>().unwrap()).collect();
    let (low, high) = (range[0], range[1]);
    let mid = iter.next().unwrap();
    let pwd = iter.next().unwrap();

    let p = Password {
        low,
        high,
        c: mid.chars().nth(0).unwrap(),
        pwd: pwd.to_string(),
    };
    p
}

fn valid_count(line: &str) -> bool {
    let p = parse_password(line);
    let mut count = 0;
    for x in p.pwd.chars() {
        if x == p.c {
            count += 1;
        }
    }
    p.low <= count && count <= p.high
}

fn num_valid(lines: std::io::Lines<io::BufReader<std::fs::File>>, is_valid: &dyn Fn(&str) -> bool) -> i32 {
    let mut valid = 0;
    for l in lines {
        if is_valid(&l.unwrap()) {
            valid += 1;
        }
    }
    valid
}

fn valid_position(line: &str) -> bool {
    let p = parse_password(line);
    let x = p.pwd.as_bytes()[(p.low-1) as usize] as char;
    let y = p.pwd.as_bytes()[(p.high-1) as usize] as char;

    1 == (((p.c == x) as i32) + ((p.c == y) as i32))
}

fn main() {
    let example_lines = get_lines("example.input");
    println!("{}", num_valid(example_lines, &valid_count));
    let problem_lines = get_lines("problem.input");
    println!("{}", num_valid(problem_lines, &valid_count));
    let problem_lines = get_lines("problem.input");
    println!("{}", num_valid(problem_lines, &valid_position));
}
