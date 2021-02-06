#!/usr/bin/env bash
for i in "$@"
do
   echo $i
   python3 ./src/lexer.py $i
   echo "<----------------------------------------------------------------------->"
done
