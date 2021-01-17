fn printa(x:i32) -> bool
{
    if(x == 0)
    {
        return false;
    }
    else
    {
        return true;
    }
}

fn main()
{
  if(printa(0) || printa(1)){
    println!(3);
  } 
  else
  {
    println!(42);
  }

  return;
}