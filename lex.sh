#!/bin/bash

STATUS=0
if [ $# -eq 1 ] && ([ "$1" == "-h" ] || [ "$1" == "--help" ]); then
   echo "List all the programs that you want to test as $ bash lex.sh tests/test1.c tests/test2.c ... "
else
   export lex_env=1
   for i in "$@"
   do
      echo $i
      python3 ./src/lexer.py $i
      RETVAL=$?
      if [ $RETVAL -ne 0 ]; 
      then
        STATUS=$RETVAL
      fi;
      echo "<----------------------------------------------------------------------->"
   done
fi
exit $STATUS
