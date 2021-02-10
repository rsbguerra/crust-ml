release=--profile release
test-file=./tests/test


all:
	dune build $(release) ./crust.exe

print:
	dune exec  $(release) -- ./crust.exe --print-ast $(test).rs

print-tast:
	dune exec  $(release) -- ./crust.exe --print-tast $(test).rs

print-past:
	dune exec  $(release) -- ./crust.exe --print-past $(test).rs
	
test-s:
	dune exec $(release) -- ./crust.exe $(test).rs

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

clean:
	dune clean
