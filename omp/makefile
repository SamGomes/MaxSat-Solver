all : maxsat-omp 

maxsat-omp :  maxsat-omp.o
	gcc -fopenmp -O0 -o maxsat-omp maxsat-omp.o -lm

maxsat-omp.o : maxsat-omp.c
	gcc -fopenmp -c maxsat-omp.c -O0

ex : clean maxsat-omp 
clean:
	rm -f *.o *.out maxsat-omp



