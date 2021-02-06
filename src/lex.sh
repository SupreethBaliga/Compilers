#!/usr/bin/env bash
for i in "$@"
do
   echo $i
   python3 lexer.py $i
   echo "<----------------------------------------------------------------------->"
done
