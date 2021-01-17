fn add(a:&i32, b:&i32) -> i32
{
  return (*a) + (*b);
}

fn main()
{
  let mut x:i32 = 52;
  let mut y:&mut i32 = &mut x;
    
  let y:&mut &mut i32 = &mut y;
  

  println!(*y);
  
  return;
}
