fn test(x:&i32)
{
  return;
}

fn main()
{
  let x:i32 = 12;
  
  test(&x);

  println!(x);
  
  return;
}
