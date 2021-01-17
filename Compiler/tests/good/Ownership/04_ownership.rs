fn main()
{
  let x:i32 = 12;
  let x:&i32 = &x;
  
    
  let y:& & i32 = &x;
  

  println!( *y );
  
  return;
}