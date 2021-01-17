fn main()
{
  let x:i32 = 12;
  let x:&i32 = &x;
  let x:& & i32 = &x;
    
  println!( *x );
  
  return;
}