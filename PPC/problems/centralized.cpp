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
    // Sent this permutation to a free slave.
    MPI_Recv(NULL, 0, MPI_INT, MPI_ANY_SOURCE, FREE_SLAVE, MPI_COMM_WORLD, &status);
    // We wait for any slave to tell us that he is free and then we give him the permutation.
    int slave_id = status.MPI_SOURCE;
    MPI_Send(a, n, MPI_INT, slave_id, ADD_PERMUTATION, MPI_COMM_WORLD);
  }
  else
  {
    for (j = i; j < n; j++)
    {
      swap(a[i], a[j]);
      permute(a, i+1, n);
      swap(a[i], a[j]); //backtrack
    }
  }
} 

int master_code(int numprocs) {
  float min_cost = INFINITY;
  int* best_permutation = (int*)malloc(n * sizeof(int));
  int* permutation = (int*)malloc(n * sizeof(int));
  for (int i = 0; i < n; ++i) {
    permutation[i] = i;
  }
  permute(permutation, 1, n); 
  // Kill the slaves
  for (int i = 1; i < numprocs; ++i) {
    float slave_cost;
    // We send our minimum cost and receive the slave's minimum.
    MPI_Sendrecv(&min_cost, 1, MPI_FLOAT, i, KILL_SLAVE, &slave_cost, 1, MPI_INT, i, RESULT, MPI_COMM_WORLD, NULL);
    if (slave_cost < min_cost) {
      min_cost = slave_cost;
      MPI_Recv(best_permutation, n, MPI_INT, i, UPDATE_RESULT, MPI_COMM_WORLD, NULL);
    }
  }
  printf("Best result %f\n", min_cost);
  for(int i = 0; i < n; ++i) {
    printf("%d ", best_permutation[i] + 1);  
  }
  printf("\n");
}

int slave_code() {
  MPI_Status status;
  int index;
  float min_cost = INFINITY;
  int* best_permutation = (int*)malloc((n) * sizeof(int));
  int* work = (int*)malloc((n) * sizeof(int));
  while (true) {
    // Tell the master that we are free
    MPI_Send(NULL, 0, MPI_INT, 0, FREE_SLAVE, MPI_COMM_WORLD);
    // The master tells us what to do.
    MPI_Recv(work, n, MPI_INT, 0, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
    // If the master tells us they are finished then we just need to deliver the result.
    if (status.MPI_TAG == KILL_SLAVE) {
      // We send the master our minimum costs so that he knows as well wether to expect or not
      // a permutation from us.
      MPI_Send(&min_cost, 1, MPI_FLOAT, 0, RESULT, MPI_COMM_WORLD);
      if (work[0] > min_cost) {
        // If necessary send the permutation.
        MPI_Send(best_permutation, n, MPI_INT, 0, UPDATE_RESULT, MPI_COMM_WORLD);
      }
      return 0;
    }
    // Otherwise we need to process the permutation.
    float cost = 0;
    for (int i = 0; i < n - 1; ++i) {
      cost += graph[work[i]][work[i+1]];
    }
    cost += graph[work[n-1]][work[0]];
    if (cost < min_cost) {
      min_cost = cost;
      for (int i = 0; i < n; ++i) {
        best_permutation[i] = work[i];
      }
    }
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

  if (id == 0) { // we are in the master
    master_code(numprocs);
  } else { // we are in the slave
    slave_code();
  }

  free2dfloat(&graph);
  stop = MPI_Wtime();
  if (id == 0) {
    printf("Time elapsed is %.2f\n", stop - start);
  }

  MPI_Finalize();
  return 0;
}

