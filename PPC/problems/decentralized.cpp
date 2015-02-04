#include <stdio.h>
#include <cstdlib>
#include <stdlib.h>
#include <queue>
#include <mpi.h>

#define INFINITY (999999)

#define KILL_SLAVE 1
#define INDEX 3
#define ADD_PERMUTATION 4
#define FREE_SLAVE 5
#define RESULT 6
#define UPDATE_RESULT 7


using namespace std;

int n;
float **graph;
float min_cost = INFINITY;
int* best_permutation;

int malloc2dfloat(float ***array, int n, int m) {
  /* allocate the n*m contiguous items */
  float *p = (float *)malloc(n*m*sizeof(float));
  if (!p) return -1;

  /* allocate the row pointers into the memory */
  (*array) = (float **)malloc(n*sizeof(float*));
  if (!(*array)) {
    free(p);
    return -1;
  }

  /* set up the pointers into the contiguous memory */
  for (int i=0; i<n; i++) 
    (*array)[i] = &(p[i*m]);

  return 0;
}

void swap(int &x, int &y) {
  int aux = x;
  x = y;
  y = aux;
}

int free2dfloat(float ***array) {
  /* free the memory - the first element of the array is at the start */
  free(&((*array)[0][0]));
  /* free the pointers into the memory */
  free(*array);
  return 0;
}

void read_data() {
  FILE* fp = fopen("input.txt", "r");
  if (fp == NULL) {
    fprintf(stderr, "Cannout open file input.txt\n");
    exit(1);
  }
  fscanf(fp, "%d", &n);
  malloc2dfloat(&graph, n, n);
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      fscanf(fp, "%f", &graph[i][j]);
    }
  }
}

void permute(int a[], int i, int n) {
  int j; 
  MPI_Status status;
  if (i == n) {
    float cost = 0;
    for (i = 0; i < n - 1; ++i) {
      cost += graph[a[i]][a[i+1]];
    }
    cost += graph[a[n-1]][a[0]];
    if (cost < min_cost) {
      min_cost = cost;
      for (i = 0; i < n; ++i) {
        best_permutation[i] = a[i];
      }
    }
  }
  else
  {
    for (int j = i; j < n; j++)
    {
      swap(a[i], a[j]);
      permute(a, i+1, n);
      swap(a[i], a[j]); //backtrack
    }
  }
} 

void compute_cost(int rank, int numprocs) {
  best_permutation = (int*)calloc(n ,sizeof(int));
  int* permutation = (int*)malloc(n * sizeof(int));
  for (int i = 0; i < n; ++i) {
    permutation[i] = i;
  }

  for (int i = 1; i < n; ++i) {
    // This is where the split happens.
    if ((i % numprocs) != rank) {
      continue;
    }
    swap(permutation[1], permutation[i]);
    permute(permutation, 2, n); 
    swap(permutation[i], permutation[1]);
  }
}

int main (int argc, char* argv[]) {
  int id, numprocs;

  double start, stop;
  start = MPI_Wtime();
    
  MPI_Init (&argc, &argv);  /* starts MPI */
  MPI_Comm_rank (MPI_COMM_WORLD, &id);  /* get current process id */
  MPI_Comm_size (MPI_COMM_WORLD, &numprocs);  /* get number of processes */
  
  if (id == 0) { // we are in the master
    read_data();
  }
  // Send the number of nodes to each of the slaves.
  MPI_Bcast(&n, 1, MPI_INT, 0, MPI_COMM_WORLD);
  if (id != 0) { // we are in the slave
    malloc2dfloat(&graph, n, n);
  }
  // Broadcast the matrix
  MPI_Bcast(&(graph[0][0]), n * n, MPI_INT, 0, MPI_COMM_WORLD);

  compute_cost(id, numprocs);
  // We gather all the minimum
  float* minimums = (float*)calloc(numprocs, sizeof(float));
  MPI_Allgather(&min_cost, 1, MPI_FLOAT, minimums, 1, MPI_FLOAT, MPI_COMM_WORLD);
  int min_index = 0;
  for (int i = 1; i < numprocs; ++i) {
    if (minimums[min_index] > minimums[i]) {
      min_index = i;
    }
  }
  if (id == min_index) {
    printf("Best result %f\n", min_cost);
    for(int i = 0; i < n; ++i) {
      printf("%d ", best_permutation[i] + 1);  
    }
    printf("\n");
  }

  free2dfloat(&graph);

  stop = MPI_Wtime();
  if (id == 0) {
    printf("Time elapsed is %.2f\n", stop - start);
  }

  MPI_Finalize();
  return 0;
}

