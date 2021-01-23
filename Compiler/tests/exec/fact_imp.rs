fn fact_imp(mut n: i32) -> i32 {
    let mut res = 1;
    while n > 1 {
        res = res * n;
        n = n - 1;
    }
    res
}

fn main() {
  if fact_imp(0) == 1 { print!("fact(0) = 1\n") }
  if fact_imp(1) == 1 { print!("fact(1) = 1\n"); }
  if fact_imp(5) == 120 { print!("fact(5) = 120\n"); }
  if fact_imp(12) == 479001600  { print!("fact(12) = 479001600\n"); }
}
