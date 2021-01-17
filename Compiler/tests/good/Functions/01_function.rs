fn y() -> i32{
    return 42;
}

fn x() -> i32 {
    return y();
}

fn main(){
    println!(x());

    return;
}