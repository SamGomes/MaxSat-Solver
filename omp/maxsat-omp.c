
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <omp.h>

typedef struct lst_item{
   int first;
   int second;
} pii;

void treeDFS(int** mainHash);
void convertToAssignment(int var,int binary[]);
//------------------------------------

pii testConditions(int** clauses,int testValue,int partialSatisfiedConditions[]);
pii initConditions(int** clauses,int* testValue,int size,int partialSatisfiedConditions[]);


int numVars;
int numClauses;

int* possibleAssignment;

pii result;


int main(int argc, char* argv[]){

    FILE* fptr;

    fptr = fopen(argv[1], "r");
    if(fptr == NULL) {
        printf("Error opening file.\n");
        exit(-1);
    }
    if(fscanf(fptr, "%d %d", &numVars, &numClauses)==1){};


    int** mainHash = (int**) malloc(sizeof(int*)*numClauses);

    int i;

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


    printf("%d %d\n", result.second,result.first);
    for(i=0;i<numVars;i++)
        printf("%d ", possibleAssignment[i] ? i+1 : -(i+1));
    printf("\n");
	

    // Free what needs to be freed
    for(i=0; i<numClauses; i++) {
        free(mainHash[i]);
    }
    free(mainHash);
    free(possibleAssignment);

    
}




//----------------tree functions-----------------



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
        
        #pragma omp critical
        {
            if(result.second==sat){
                result.first++;
            }else if(result.second<sat) {
                result.first=1;
                result.second= sat;
                memcpy(possibleAssignment,assignment,sizeof(int)*numVars);
            }
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

void treeDFS(int** mainHash){
	
    int i;
    
    int exponent=log2(omp_get_max_threads())+4;
    if(exponent>numVars) 
        exponent = numVars;
    int numTasks = 1 << exponent;


  int binary[numVars];
  int conditions[numClauses];

    #pragma omp parallel for  private(binary,conditions)  schedule(dynamic)
    for(i=0;i<numTasks;i++){


      memset( conditions, 0, numClauses*sizeof(int) );
      memset( binary, 0, numVars*sizeof(int) );

      convertToAssignment(i,binary);

      int sat = initConditions(mainHash,binary,exponent,conditions).first;
     
      DFSAux(mainHash,exponent+1,sat,conditions,binary);
      

    }

    
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
