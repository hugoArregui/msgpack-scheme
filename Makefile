test : clean

	csc -X bind -c++ -s -j msgpack-imple msgpack-imple.scm
	csc -s msgpack-imple.import.scm
	csc -c tests/utils.scm
	csc -extend msgpack-imple.import.so tests/utils.o tests/run.scm -o run 

clean :

	rm -f tests/*.o *.o run *.c tests/*.c *.so
