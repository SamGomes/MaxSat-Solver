#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <omp.h>
#include <mpi.h>



//----------tags------------------------------
#define TAG_SEND_TASKS 1

#define BLOCK_LOW(id,p,n)  ((id)*(n)/(p))
#define BLOCK_HIGH(id,p,n) (BLOCK_LOW((id)+1,p,n)-1)
#define BLOCK_SIZE(id,p,n) (BLOCK_HIGH(id,p,n)-BLOCK_LOW(id,p,n)+1)
#define BLOCK_OWNER(index,p,n) (((p)*((index)+1)-1)/(n))


typedef struct pii_items{
   int first;
   int second;
} pii;


//----------function declarations-------------
void treeDFS(int**);
void DFSAux(int**, int, int, int*, int* );

pii testConditions(int**, int, int*);
pii initConditions(int**, int*, int, int*);

void convertToAssignment(int, int*);



//----------global vars---------------------
int numVars;
int numClauses;
int* possibleAssignment;
int nprocs;
int pid;

pii result;


int main(int argc, char* argv[]) {

	
    MPI_Init(&argc, &argv);
    int i;
    double elapsed_time;

	MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
	MPI_Comm_rank(MPI_COMM_WORLD, &pid);



    FILE* fptr = fopen(argv[1], "r");
    if(fptr == NULL) {
        printf("Error opening file.\n");fflush(stdout);
        exit(-1);
    }
    if(fscanf(fptr, "%d %d", &numVars, &numClauses)==1){};


    int** mainHash = (int**) malloc(sizeof(int*)*numClauses);


    for(i=0;i<numClauses;i++){

        mainHash[i] = (int*) malloc(sizeof(int)*(numVars+1));

        int tempNum=-1;
        int oldTempNum;
        int absTempNum;

        while(tempNum!=0){
            if(fscanf(fptr, "%d", &tempNum)==1){};
            if(tempNum!=0){
                mainHash[i][abs(tempNum)-1]=tempNum;
                oldTempNum = mainHash[i][numVars];
                absTempNum = abs(tempNum);
                if(absTempNum>oldTempNum)
                    mainHash[i][numVars]=absTempNum;
            }
            
        }
    }
    
	fclose(fptr);

    possibleAssignment = (int*)malloc(sizeof(int)*numVars);

    treeDFS(mainHash);


    if(!pid) {
        printf("%d %d\n", result.second,result.first);
        fflush(stdout);
        for(i=0;i<numVars;i++) {
            printf("%d ", possibleAssignment[i] ? i+1 : -(i+1));
            fflush(stdout);
        }
        printf("\n");
        fflush(stdout);
    }

    // Free what needs to be freed
    for(i=0; i<numClauses; i++) {
		free(mainHash[i]);
	}
	free(mainHash);
    free(possibleAssignment);

	MPI_Finalize();
}

void treeDFS(int** mainHash) {


    MPI_Status status;
    MPI_Request request;

    int i;
    

    int exponent=log2(nprocs)+4;

    if(exponent>numVars) 
        exponent = numVars;
    int numTasks = 1 << exponent;



    /* DIVIDE THE ARRAY THROUGHOUT THE TASKS */
    int higherBound = BLOCK_HIGH(pid, nprocs, numTasks);

    int lowerBound = BLOCK_LOW(pid, nprocs, numTasks);

    int* binary=(int*) malloc(sizeof(int)*numVars);
    int* conditions=(int*) malloc(sizeof(int)*numClauses);

    for(i=lowerBound;i<=higherBound;i++){
		
        memset( conditions, 0, numClauses*sizeof(int) );
        memset( binary, 0, numVars*sizeof(int) );

		convertToAssignment(i,binary);

		int sat = initConditions(mainHash,binary,exponent,conditions).first;

		DFSAux(mainHash,exponent+1,sat,conditions,binary);
        
   }



    free(binary);
    free(conditions);

    int localResult[2];

    localResult[0]=result.first;
    localResult[1]=result.second;

	if(nprocs > 1) {
	
		if(!pid){

		    MPI_Send(localResult, 2, MPI_INT, 1, 0, MPI_COMM_WORLD);
		    MPI_Recv(localResult, 2, MPI_INT, nprocs-1, nprocs-1, MPI_COMM_WORLD, &status);

		    result.first = localResult[0];
		    result.second = localResult[1];

		}else{

		    MPI_Recv(localResult, 2, MPI_INT, pid-1, pid-1, MPI_COMM_WORLD, &status);

		    if(localResult[1]==result.second){
		        localResult[0]+=result.first;
		    }
		    if(localResult[1]<result.second){
		        localResult[1]=result.second;
		        localResult[0]=result.first;
		    }

		    result.first = localResult[0];
		    result.second = localResult[1];

		    MPI_Send(localResult, 2, MPI_INT, (pid+1)%nprocs, pid, MPI_COMM_WORLD);
		}
	}
    
}

void DFSAux(int** mainHash,int curr,int sat,int inConditions[],int assignment[]){

    int i;  
    int* partialResult;
    pii testCond;

    int auxConditions[numClauses];

    
    memcpy(auxConditions,inConditions,sizeof(int)*numClauses);
    
    if(curr > 1){

        testCond=testConditions(mainHash,assignment[curr-2] ? curr-1 : -(curr-1),inConditions);
        sat+=testCond.first;
        int i;
        
    }

    if(curr==numVars+1){
        
        if(result.second==sat){
            result.first++;
        }else if(result.second<sat) {
            result.first=1;
            result.second= sat;
            memcpy(possibleAssignment,assignment,sizeof(int)*numVars);
        }

        memcpy(inConditions,auxConditions,sizeof(int)*numClauses);
        return;
    } 
    if(curr > 1){
        if(testCond.second > numClauses- result.second){
            memcpy(inConditions,auxConditions,sizeof(int)*numClauses);
            return;
        }
    }

    assignment[curr-1] = 0;
    DFSAux(mainHash,curr+1,sat,inConditions,assignment);

    assignment[curr-1] = 1;
    DFSAux(mainHash,curr+1,sat,inConditions,assignment);

    memcpy(inConditions,auxConditions,sizeof(int)*numClauses);
    
}



void convertToAssignment(int var,int binary[]){

    int i;
    for(i=0;i<numVars;i++){
        binary[i]=(var>>i)%2; 
    }

}



pii testConditions(int** clauses,int testValue,int partialSatisfiedConditions[]){

    pii result;

    int varSat=0;
    int i;
    int currAssignTest;
    int currValue;
    int unsatisfiedClauses = 0;
    for(i=0;i<numClauses;i++){

        currAssignTest=partialSatisfiedConditions[i];

        if(currAssignTest==0){

            currValue = clauses[i][abs(testValue)-1];

            if(currValue == testValue){
                partialSatisfiedConditions[i]=1;
                varSat++;
                
            }else
                if(abs(testValue)==abs(clauses[i][numVars])){
                    partialSatisfiedConditions[i]=2;
                    unsatisfiedClauses++;
                }

        }
        else

            if(currAssignTest==2)
                unsatisfiedClauses++;

        
    }

    result.first=varSat;
    result.second=unsatisfiedClauses;
    
    return result;
}

pii initConditions(int** clauses,int* testValue,int size,int partialSatisfiedConditions[]){

    pii result;

    int varSat=0;
    int i;
    int j;
    int currAssignTest;
    int currValue;
    int unsatisfiedClauses = 0;
    int testValueAux;

    for(j=0;j<size;j++){

        testValueAux = testValue[j]==1 ? (j+1) : -(j+1); 

        for(i=0;i<numClauses;i++){

            currAssignTest=partialSatisfiedConditions[i];

            if(currAssignTest==0){

                currValue = clauses[i][abs(testValueAux)-1];

                if(currValue == testValueAux){
                    partialSatisfiedConditions[i]=1;
                    varSat++;
                    
                }else
                    if(abs(testValueAux)==abs(clauses[i][numVars])){
                        partialSatisfiedConditions[i]=2;
                        unsatisfiedClauses++;
                    }

            }
            else

                if(currAssignTest==2)
                    unsatisfiedClauses++;

            
        }
    }

    result.first=varSat;
    result.second=unsatisfiedClauses;
    
    return result;
}