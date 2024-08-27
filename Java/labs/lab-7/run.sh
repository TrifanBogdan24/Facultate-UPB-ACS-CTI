cd src/ || exit 1

javac Main.java || exit 1

echo "1" | java Main > ../res/out1
echo "2" | java Main > ../res/out2

rm -f *.class

cd ../

diff ref/ref1 res/out1
diff ref/ref2 res/out2
	
