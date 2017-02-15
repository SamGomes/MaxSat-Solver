all : maxsat-mpi 

maxsat-mpi : 
	mpicc -fopenmp -O0 -o maxsat-mpi maxsat-mpi.c -lm

clean:
	rm -f *.o *.out maxsat-mpi


