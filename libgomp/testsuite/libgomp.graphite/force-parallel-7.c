#define N 500

int A[N+5][N+5][N+5];

void abort (void);

int foo (void)
{
  int i, j, k;

  for (i = 0; i < N + 5; i++)
    for (j = 0; j < N + 5; j++)
      for (k = 0; k < N + 5; k++)
	A[i][j][k] = i + j + k;

  /* Loop i: carried no dependency.  */
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      for (k = 0; k < N; k++)
	A[k+1][j+2][i+1] = A[k][j][i+1];

  return A[1][5][2];
}

int main (void)
{
  if (5 != foo ())
    abort ();

  return 0;
}

/* Check that parallel code generation part make the right answer.  */
/* { dg-final { scan-tree-dump-times "5 loops carried no dependency" 1 "graphite" } } */
/* { dg-final { scan-tree-dump-times "loopfn.0" 4 "optimized" } } */
