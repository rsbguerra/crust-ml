#!/bin/bash
DIR="$(dirname "${BASH_SOURCE[0]}")"
DIR='./tests'
echo $DIR
shopt -s nullglob

# script para testar o trabalho de DLPC

option=$1
compilo=$2
score=0
max=0
verbose=0


# echo "Test de $2"

echo

# todos os testes passam com rustc
test_rustc() {
echo -n "syntax... "
for f in $DIR/syntax/bad/*.rs; do
    if rustc $f -o a.out > /dev/null 2>&1 ; then
      echo "rustc: success over $f"; exit 1
    fi
done
echo "OK"

echo -n "typing... "
for f in $DIR/syntax/good/*.rs $DIR/$DIR/typing/good/*.rs $DIR/typing2/good/*.rs $DIR/exec/*.rs $DIR/exec-fail/*.rs; do
    rustc --emit=dep-info $f -o a.out  > /dev/null 2>&1 ||
     (echo "rustc : failure over $f"; exit 1)
done
for f in $DIR/typing/bad/*.rs typing2/bad/*.rs; do
    if rustc $f -o a.out > /dev/null 2>&1 ; then
      echo "succ�s de rustc sur $f"; exit 1
    fi
done
echo "OK"

echo "exec"
for f in exec/*.rs; do
    echo "  $f"
    expected=exec/`basename $f .rs`.out
    if rustc $f -o a.out > /dev/null 2>&1 ; then
      ./a.out > out
      if ! cmp --quiet out $expected; then
          echo "rustc : wrong output over $f"; exit 1
      fi
    else
      echo "rustc: failure over $f"; exit 1
    fi
done

echo "exec-fail"
for f in exec-fail/*.rs; do
    echo "  $f"
    expected=exec/`basename $f .rs`.out
    if rustc $f -o a.out > /dev/null 2>&1 ; then
      if ./a.out > /dev/null 2>&1 ; then
          echo "rustc: no failure over $f"; exit 1
      fi
    else
      echo "rustc: failure over $f"; exit 1
    fi
done
}

compile () {
if [[ $verbose != 0 ]]; then
  echo Compile $1 $2
  "$DIR/../_build/default/crust.exe" $1 $2;
else
  "$DIR/../_build/default/crust.exe" $1 $2 > /dev/null 2>&1;
fi;
}


# parte 1 : testes  para a analise sint�ctica

partie1 () {

score=0
max=0

echo "Parte 1"

# os maus
echo -n "bad ones "
for f in $DIR/syntax/bad/*.rs; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --parse-only $f;
    case $? in
	"0")
	echo
	echo "FAILURE over "$f" (should fail)";;
	"1") score=`expr $score + 1`;;
	*)
	echo
	echo "FAILURE over "$f" (for a bad reason)";;
    esac
done
echo

# os bons
echo -n "good ones "
for f in $DIR/syntax/good/*.rs $DIR/typing/bad/*.rs $DIR/typing/good/*.rs $DIR/typing2/bad/*.rs $DIR/typing2/good/*.rs $DIR/exec/*.rs $DIR/exec-fail/*.rs; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --parse-only $f;
    case $? in
	"1")
	echo
	echo "FAILURE over "$f" (should succeed)";;
	"0") score=`expr $score + 1`;;
	*)
	echo
	echo "FAILURE over "$f" (for a bad reason)";;
    esac
done
echo

percent=`expr 100 \* $score / $max`;

echo -n "Syntax : $score/$max : $percent%"; }

# parte 2 : testes da an�lise sem�ntica


partie2 () {
echo
echo "Parte 2"

score=0
max=0

# os maus
echo -n "bad ones "
for f in $DIR/typing/bad/*.rs; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --type-only $f;
    case $? in
	"0")
	echo
	echo "FAILURE over "$f" (should fail)";;
	"1") score=`expr $score + 1`;;
	*)
	echo
	echo "FAILURE over "$f" (for a bad reason)";;
    esac
done
echo

# os bons
echo -n "good ones "
for f in $DIR/typing/good/*.rs $DIR/typing2/bad/*.rs $DIR/typing2/good/*.rs $DIR/exec/*.rs $DIR/exec-fail/*.rs; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --type-only $f;
    case $? in
	"1")
	echo
	echo "FAILURE over "$f" (should succeed)";;
	"0") score=`expr $score + 1`;;
	*)
	echo
	echo "FAILURE over "$f" (for a bad reason)";;
    esac
done
echo

percent=`expr 100 \* $score / $max`;

echo    "Typing  : $score/$max : $percent%";
}

partie2b () {
echo
echo "Parte 2b"

score=0
max=0

# os maus
echo -n "bad ones "
for f in $DIR/typing2/bad/*.rs; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --no-asm $f;
    case $? in
	"0")
	echo
	echo "FAILURE over "$f" (should fail)";;
	"1") score=`expr $score + 1`;;
	*)
	echo
	echo "FAILURE over "$f" (for a bad reason)";;
    esac
done
echo

# os bons
echo -n "good ones "
for f in $DIR/typing2/good/*.rs $DIR/exec/*.rs $DIR/exec-fail/*.rs; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --no-asm $f;
    case $? in
	"1")
	echo
	echo "FAILURE over "$f" (should succeed)";;
	"0") score=`expr $score + 1`;;
	*)
	echo
	echo "FAILURE over "$f" (for a bad reason)";;
    esac
done
echo

percent=`expr 100 \* $score / $max`;

echo    "Typing  : $score/$max : $percent%";
}


# parte 3 : testes de execu��o

partie3 () {

score_comp=0
score_out=0
score_test=0
max=0

echo
echo "Part 3"
echo "normal execution"
echo "-----------------"


timeout=""

for f in $DIR/exec/*.rs; do
    echo -n "."
    asm=$DIR/exec/`basename $f .rs`.s
    rm -f $asm
    expected=$DIR/exec/`basename $f .rs`.out
    max=`expr $max + 1`;
    if compile $f; then
	rm -f out
	score_comp=`expr $score_comp + 1`;
	if gcc -g -no-pie -g -no-pie $asm && ./a.out > out; then
	    score_out=`expr $score_out + 1`;
	    if cmp --quiet out $expected; then
		score_test=`expr $score_test + 1`;
	    else
		echo
		echo "FAILURE : bad output for $f"
	    fi
	else
		echo
		echo "FAILURE of code generation for $f"
	fi
    else
	echo
	echo "FAILURE of the compilation for $f (should succeed)"
    fi
done
echo

echo "Execution driving to a failure"
echo "-------------------------------"

for f in $DIR/exec-fail/*.rs; do
    echo -n "."
    asm=$DIR/exec-fail/`basename $f .rs`.s
    rm -f $asm
    max=`expr $max + 1`;
    if compile $f && gcc -no-pie $asm; then
	score_comp=`expr $score_comp + 1`;
	if { ./a.out; } > /dev/null 2>&1; then
	    echo
	    echo "FAILURE : the code $f should fail"
	else
	    score_test=`expr $score_test + 1`;
	    score_out=`expr $score_out + 1`;
	fi
    else
	echo
	echo "FAILURE of the compilation of $f (should succeed)"
    fi
done

echo
percent=`expr 100 \* $score / $max`;

echo "Compilation:";
percent=`expr 100 \* $score_comp / $max`;
echo "Compilation : $score_comp/$max : $percent%";
percent=`expr 100 \* $score_out / $max`;
echo "Produced code : $score_out/$max : $percent%";
percent=`expr 100 \* $score_test / $max`;
echo "Code behaviour : $score_test/$max : $percent%";}

case $option in
    "-1" )
        partie1;;
    "-2" )
        partie2;;
    "-2b" )
        partie2b;;
    "-3" )
        partie3;;
    "-v1" )
	verbose=1;
	partie1;;
    "-v2" )
    	verbose=1;
        partie2;;
    "-v2b" )
    	verbose=1;
        partie2;;
    "-v3" )
    	verbose=1;
        partie3;;
    "-all" )
    	partie1;
    	partie2;
    	partie2b;
    	partie3;;
    "-rustc" )
        test_rustc;;
    * )
        echo "usage : $0 <option> <compilo>"
        echo "choose one option from : "
        echo "-1      : test part 1"
        echo "-2      : test part 2"
        echo "-2b     : test part 2b"
        echo "-3      : test part 3"
        echo "-v1     : test part 1 (verbose)"
        echo "-v2     : test part 2 (verbose)"
        echo "-v3     : test part 3 (verbose)"
        echo "-all    : teste everything";;

esac
echo
