use std::fs::File;
use std::path::Path;
use std::io::{self, BufRead};
use std::vec::Vec;
use std::collections::{HashSet, HashMap};

fn read_file(filename: &str) -> Result<Vec<i32>, std::io::Error> {
    let path = Path::new(&filename);
    let file = File::open(path)?;
    let lines = io::BufReader::new(file).lines();
    
    let mut nums = Vec::new();
    for line in lines {
        if let Ok(l) = line {
            nums.push(l.parse::<i32>().unwrap());
        }
    }

    Ok(nums)
}

fn two_sum(nums: &[i32], sum: i32) -> (i32, i32) {
    let mut prev_nums = HashSet::new();
    for &x in nums {
        let y = sum - x;
        if prev_nums.contains(&y) {
            return (x, y)
        }
        prev_nums.insert(x);
    }
    panic!("no two_sum found!");
}

fn three_sum(nums: &[i32], sum: i32) -> (i32, i32, i32) {
    let mut all_nums: HashMap<i32, i32> = HashMap::new();
    for &x in nums {
        all_nums.insert(x, all_nums.get(&x).unwrap_or(&0) + 1);
    }

    for i in 1..(nums.len()) {
        for j in 0..i {
            let (x, y) = (nums[i], nums[j]);

            let z = sum - x - y;
            let min_count = 1 + ((x == z) as i32) + ((y == z) as i32);
            if let Some(count) = all_nums.get(&z) {
                if *count >= min_count {
                    return (x, y, z)
                }
            }
        }
    }

    panic!("no three_sum found!");
}

fn main() {
    let nums = read_file("day1.input").unwrap();
    let (x, y) = two_sum(&nums, 2020);
    println!("two_sum has result of {:?}, their product is {}", (x, y), x*y);

    let (x, y, z) = three_sum(&nums, 2020);
    println!("three_sum has result of {:?}, their product is {}", (x, y, z), x*y*z);
}
