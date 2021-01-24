release=--profile release
test=./tests/test

all:
	dune build $(release) ./crust.exe

print:
	dune exec  $(release) -- ./crust.exe --print-ast $(test).rs

print-tast:
	dune exec  $(release) -- ./crust.exe --print-tast $(test).rs

print-past:
	dune exec  $(release) -- ./crust.exe --print-past $(test).rs

test:
	dune build $(release) ./crust.exe
	bash ./tests/run-tests

test-s:
	dune exec $(release) -- ./crust.exe $(test).rs

clean:
	dune clean
