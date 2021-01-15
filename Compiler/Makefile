release=--profile release
test=./tests/test

all:
	dune build $(release) ./crust.exe

print:
	dune exec  $(release) -- ./crust.exe -print-ast $(test)

print-tast:
	dune exec  $(release) -- ./crust.exe -print-tast $(test)

print-past:
	dune exec  $(release) -- ./crust.exe -print-past $(test)

test:
	bash ./tests/run-tests

test-s:
	dune exec $(release) -- ./crust.exe $(test).rs
	gcc -g -no-pie $(test).s
	./a.out

clean:
	dune clean
