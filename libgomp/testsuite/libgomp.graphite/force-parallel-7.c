#define N 500

int A[N+5][N+5][N+5];

int foo(void)
{
  int i, j, k;

  /* Loop i: carried no dependency.  */
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      for (k = 0; k < N; k++)
	A[k+1][j+2][i+1] = A[k][j][i+1];

  for (i = 0; i < N; i++)
    /* Loop j: carried no dependency.  */
    for (j = 0; j < N; j++)
      /* Loop k: carreid no dependency.  */
      for (k = 0; k < N; k++)
	A[i+1][j][k] = A[i][j][k+1];

  return A[1][5][2];
}

int main(void)
{
  foo();

  return 0;
}

/* Check that parallel code generation part make the right answer.  */
/* { dg-final { scan-tree-dump-times "3 loops carried no dependency" 1 "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
/* { dg-final { scan-tree-dump-times "loopfn.0" 5 "optimized" } } */
/* { dg-final { cleanup-tree-dump "parloops" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
