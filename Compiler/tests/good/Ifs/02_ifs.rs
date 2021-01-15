fn printa(x:i32) -> i32{
    if(x == 0){
        return 0;
    }else{
        return 1;
    }
}

fn main(){
    if(printa(0) && printa(1)){
        printn!(3);
    } else {
        println!(42);
    }
}