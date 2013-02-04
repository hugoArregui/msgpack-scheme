module: clean

	csc -X bind -c++ -s -j msgpack msgpack-module.scm

test : clean

	csc -c tests/utils.scm
	csc -X bind -c++ tests/utils.o tests/run.scm -o run 

clean :

	rm -f tests/*.o *.o run *.c tests/*.c *.so msgpack.import.scm

