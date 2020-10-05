all:
	dune build ./crust.exe

print:
	dune exec -- ./crust.exe -print-ast ./tests/good/Comments/01_comment.rs

test:
	bash ./tests/run-tests

clean:
	dune clean
