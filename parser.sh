#!/bin/bash

STATUS=0
count=0

if [ $# -eq 1 ] && ([ "$1" == "-h" ] || [ "$1" == "--help" ]); then
    echo "List all the programs that you want to test as $ bash parser.sh tests/test1.c tests/test2.c ... "
    echo "-l flag prints the scanner table"
    echo "-h[--help] flag prints this message"
    exit 0
fi

files=()
lexer=false

for i in "$@"
do
    if [ ${i:0:1} = "-" ]; then
        if [ ${i:1:2} = "l" ]; then
            lexer=true
        fi
    else
        files+=($i)
    fi
done

for i in ${files[@]};
do
    if [ ! -d "./dot" ];
    then
        mkdir dot
    fi;
    count=$((count+1))
    echo $i
    if [ $lexer = true ]; then
        export lex_env=1
    else
        export lex_env=0
    fi
    python3 ./src/parser.py $i $count
    RETVAL=$?
    if [ $RETVAL -ne 0 ]; 
    then
        echo "Failure"
    STATUS=$RETVAL
    else
        echo "Success"
        dot -Tps dot/file$count.dot -o dot/graph$count.ps
    fi;

    rm src/parsetab.py src/parser.out

    echo "<----------------------------------------------------------------------->"
done
exit $STATUS
