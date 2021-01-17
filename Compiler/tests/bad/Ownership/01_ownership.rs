fn main()
{
  let mut x:i32 = 52;
  let y:&mut i32 = &mut x;
    
  *y = 5;
  println!(x);
  
  return;
}
