#!/bin/bash

lol='1113122113'

for i in {1..50}; do
  echo -n ${lol} | wc -c
  lol=`echo ${lol} | sed 's/./\n&/g'| tail -n +2 | uniq -c | tr -d '\n '`
done

echo -n ${lol} | wc -c

