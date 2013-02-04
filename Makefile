module: clean

	csc -X bind -c++ -s flonum-utils.scm
	csc -X bind -c++ -s msgpack-imple.scm
	csc -X bind -c++ -s -j msgpack -o msgpack.so msgpack.scm
	csc msgpack.import.scm -dynamic

test : clean

	csc -c tests/utils.scm
	csc -X bind -c++ tests/utils.o tests/run.scm -o run 

clean :

	rm -f tests/*.o *.o run *.c tests/*.c *.so msgpack.import.scm

