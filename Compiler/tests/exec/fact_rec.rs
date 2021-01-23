fn fact_rec(n: i32) -> i32 {
  if n <= 1 { return 1; }
  return n * fact_rec(n - 1);
}

fn test(b: bool) {
    if b { print!("yes\n") }
}

fn main() {
  test(fact_rec(0) == 1);
  test(fact_rec(1) == 1);
  test(fact_rec(5) == 120);
  test(fact_rec(5) == 120);
  test(fact_rec(12) == 479001600);
  // 13 provoquerait un débordement (et donc une RTE en Rust)
}
