if [ ! -d diff ]; then
	# cream directorul daca nu exista deja
	mkdir diff/
fi


len="$(ls ref/*json | wc -l)"

for i in $(seq -f "%02g" 1 $len); do
	diff ref/ref_test"$i"* result/out_test"$i"* > diff/diff_"$i"

	if [ "$(cat diff/diff_"$i" | wc -l )" == 0 ]; then
		rm diff/diff_"$i"
	fi
done 

# pentru a updata ierarhia de fisiere
# si pentru a vedea schimbarile in acestea si in IDE
ls

