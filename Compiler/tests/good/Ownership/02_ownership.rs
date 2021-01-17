fn add(a:&i32, b:&i32) -> i32
{
  return (*a) + (*b);
}

fn main()
{
  let x:i32 = 52;
  let y:i32 = 107;
    
  println!(add(&x, &y));
  
  return;
}
