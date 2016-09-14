module: clean

	csc -X bind -c++ -s flonum-utils.scm
	csc -X bind -c++ -s msgpack-imple.scm
	csc -X bind -c++ -s -j msgpack -o msgpack.so msgpack.scm
	csc msgpack.import.scm -dynamic

test: clean

	csc -X bind -c++ -I tests/ tests/tests.scm -o run
	./run

clean:

	rm -f tests/*.o *.o run *.c tests/*.c *.so msgpack.import.scm

