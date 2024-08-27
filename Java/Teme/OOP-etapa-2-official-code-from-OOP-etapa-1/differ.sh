#!/bin/bash

if [ ! -d diff ]; then
  # creates the directory if it wasn't already created
	mkdir diff/
fi

# deletes the already existing files in the directory
rm -rf diff/*

len="$(ls ref/*json | wc -l)"

((len--))
for i in $(seq -f "%02g" 0 $len); do
	diff ref/ref_test"$i"* result/out_test"$i"* > diff/diff_"$i"

	if [ "$(cat diff/diff_"$i" | wc -l )" == 0 ]; then
		rm diff/diff_"$i"
	fi
done

((len++))

# ls command will update the file hierarchy in IDE
ls > fisier_output_ls.txt && rm -rf fisier_output_ls.txt
