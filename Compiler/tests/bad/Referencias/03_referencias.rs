fn add(a:&i32, b:&i32) -> i32
{
  return (*a) + (*b);
}

fn main()
{
  let mut x:i32 = 52;
  let y:i32 = x;
    
  let y: &mut i32 = y;
  

  println!(*y);
  
  return;
}
