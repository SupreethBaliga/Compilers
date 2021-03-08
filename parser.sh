#!/bin/bash

STATUS=0
count=0
if [ $# -eq 1 ] && ([ "$1" == "-h" ] || [ "$1" == "--help" ]); then
   echo "List all the programs that you want to test as $ bash parser.sh tests/test1.c tests/test2.c ... "
else
   if [ ! -d "./dot" ];
   then
      mkdir dot
   fi;
   
   for i in "$@"
   do
      count=$((count+1))
      echo $i
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
      echo "<----------------------------------------------------------------------->"
   done
fi
exit $STATUS
