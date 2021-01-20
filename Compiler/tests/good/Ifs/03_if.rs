fn printa(x:i32) -> bool
{
    return (x == 0);
}

fn main()
{
    if(printa(0) || printa(1))
    {
        let x:i32 = 12;

        if(x < 20)
        {
            println!(x);
        }
        else
        {
            println!(3);
        }
    } 
    else
    {
        println!(42);
    }

    return;
}