release=--profile release

all:
	dune build $(release) ./crust.exe

print:
	dune exec  $(release) -- ./crust.exe -print-ast ./tests/good/Comments/01_comment.rs

print-past:
	dune exec  $(release) -- ./crust.exe -print-ast ./tests/good/Comments/01_comment.rs

test:
	bash ./tests/run-tests

clean:
	dune clean
