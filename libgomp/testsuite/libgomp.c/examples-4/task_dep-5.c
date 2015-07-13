/* { dg-do run } */

#define N 128
#define BS 16
#define EPS 0.000001

#include <stdlib.h>

void matmul_depend (float A[N][N], float B[N][N], float C[N][N])
{
   int i, j, k, ii, jj, kk;
   for (i = 0; i < N; i+=BS)
     for (j = 0; j < N; j+=BS)
       for (k = 0; k < N; k+=BS)
// Note 1: i, j, k, A, B, C are firstprivate by default
// Note 2: A, B and C are just pointers
#pragma omp task private(ii, jj, kk) \
            depend ( in: A[i:BS][k:BS], B[k:BS][j:BS] ) \
            depend ( inout: C[i:BS][j:BS] )
            for (ii = i; ii < i+BS; ii++ )
              for (jj = j; jj < j+BS; jj++ )
                for (kk = k; kk < k+BS; kk++ )
                  C[ii][jj] = C[ii][jj] + A[ii][kk] * B[kk][jj];
}

void matmul_ref (float A[N][N], float B[N][N], float C[N][N])
{
   int i, j, k;

   for (i = 0; i < N; i++)
     for (j = 0; j < N; j++)
       for (k = 0; k < N; k++)
         C[i][j] += A[i][k] * B[k][j];
}

void init (float A[N][N], float B[N][N])
{
  int i, j, s = -1;
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
    {
      A[i][j] = i * j * s;
      B[i][j] = i + j;
      s = -s;
    }
}

void init_zero (float A[N][N], float B[N][N])
{
  int i, j, s = -1;
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
    {
      A[i][j] = 0;
      B[i][j] = 0;
    }
}

void check (float A[N][N], float B[N][N])
{
  int i, j;
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      if (A[i][j] - B[i][j] > EPS || B[i][j] - A[i][j] > EPS)
        abort ();
}

int main ()
{
  float A[N][N], B[N][N], C[N][N], C_ref[N][N];

  init (A, B);
  init_zero (C, C_ref);

  matmul_depend (A, B, C);
  matmul_ref (A, B, C_ref);

  check (C, C_ref);

  return 0;
}
