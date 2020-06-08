#!/bin/bash

output=$1
shift

rm -f $output
for file in $@; do
  echo "let $(basename ${file%.*}) = \"$(sed -e 's/"/\\"/g' $file)\"" >> $output
done
