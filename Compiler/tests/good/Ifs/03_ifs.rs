fn printa(x:i32) -> i32{
    if(x == 0){
        return 0;
    }else{
        return 1;
    }
}

fn main(){
    if(printa(0) || printa(1)){
        let x:i32=12;
        if x < 20{
            println!(x);
        }
        else{
            printn!(3);
        }
    } else{
        println!(42);
    }
}