release=--profile release
test-file=./tests/test


all:
	dune build $(release) ./crust.exe

print:
	dune exec  $(release) -- ./crust.exe --print-ast $(test-file).rs

print-tast:
	dune exec  $(release) -- ./crust.exe --print-tast $(test-file).rs

print-past:
	dune exec  $(release) -- ./crust.exe --print-past $(test-file).rs
	
test-s:
	dune exec $(release) -- ./crust.exe $(test-file).rs
	gcc -no-pie -g $(test-file).s ; ./a.out


test-syntax:
	dune build $(release) ./crust.exe 
	bash ./tests/test.sh -1

test-typing:
	dune build $(release) ./crust.exe 
	./tests/test.sh -2
	./tests/test.sh -2b

test-compile:
	dune build $(release) ./crust.exe 
	./tests/test.sh -3

test-all:
	dune build $(release) ./crust.exe 
	./tests/test.sh -all
clean:
	dune clean
	rm a.out 
	find tests -name \*.s -type f -delete
	