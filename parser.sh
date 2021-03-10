#!/bin/bash

STATUS=0
count=0

if [ $# -eq 0 ] || ([ $# -eq 1 ] && ([ "$1" == "-h" ] || [ "$1" == "--help" ]));
then
    echo "Usage : bash $0 /path/to/test1.c /path/to/test2.c ... "
    echo "-l : enable printing the scanner table"
    echo "-d : prevent removal of dump files of the parser"
    echo "-h / --help : prints this message"
    exit 0
fi

files=()
lexer=false
dump=false

for i in "$@"
do
    if [ ${i:0:1} = "-" ]; 
    then
        if [ ${i:1:2} = "l" ]; 
        then
            lexer=true
        elif [ ${i:1:2} = "d" ]; 
        then
            dump=true
        fi
    else
        files+=($i)
    fi
done

if [ -d "./tmp" ] && [ ! -z "$(ls -A ./tmp)" ];
then
    echo "Removing older dump files"
    rm -v tmp/*
fi;

for file in ${files[@]};
do
    count=$((count+1))
    echo "$count. $file"
    fileNameCore="${file%%.*}"
    fileNameCore=${fileNameCore##*/}

    if [ $lexer = true ]; 
    then
        export lex_env=1
    else
        export lex_env=0
    fi

    if [ ! -d "./tmp" ];
    then
      mkdir tmp
    fi
    
    python3 src/parser.py $file
    RETVAL=$?

    if [ $RETVAL -ne 0 ]; 
    then
        echo "Failure"
        STATUS=$RETVAL
    else
        echo "Success"
        dot -Tps dot/$fileNameCore.dot -o ASTgraphs/$fileNameCore.ps
    fi;

    echo "<----------------------------------------------------------------------->"
done

if [ $dump = false ] && [ ! -z "$(ls -A ./tmp)" ]; 
then
    echo "Removing parser dump files"
    rm -vr tmp/
fi;

exit $STATUS
