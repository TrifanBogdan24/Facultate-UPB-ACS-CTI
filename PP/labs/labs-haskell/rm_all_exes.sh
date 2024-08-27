#!/bin/bash

rm -f *.o *.hi

for file in *.hs ; do
    filename=$(basename "$file" .hs)
    rm -f "$filename"
done
