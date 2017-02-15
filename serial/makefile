all : maxsat-serial 

maxsat-serial :  maxsat-serial.o
	gcc -fopenmp -o maxsat-serial maxsat-serial.o

maxsat-serial.o : maxsat-serial.c
	gcc -fopenmp -c maxsat-serial.c -O0

ex : clean maxsat-serial
clean:
	rm -f *.o *.out maxsat-serial



