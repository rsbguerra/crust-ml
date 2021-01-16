fn main()
{
  let b : i32 = 42;
  let a : &i32 = &b;

  b = 5;
  println!(a);

  return;
}

