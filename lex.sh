#!/bin/bash
if [ $# -eq 1 ] && ([ "$1" == "-h" ] || [ "$1" == "--help" ]); then
   echo "List all the programs that you want to test as $ bash lex.sh tests/test1.c tests/test2.c ... "
else
   for i in "$@"
   do
      echo $i
      python3 ./src/lexer.py $i
      echo "<----------------------------------------------------------------------->"
   done
fi
