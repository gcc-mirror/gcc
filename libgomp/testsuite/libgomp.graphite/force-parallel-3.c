void abort (void);

#define N 500

int Z[2*N+2][2*N+2], B[2*N+2][2*N+2];

void foo(void)
{
  int i,j;

  for (i = 0; i < 2*N+2; i++)
    for (j = 0; j < 2*N+2; j++)
      B[i][j] = Z[i][j] = i + j;

  for (i = 0; i <= N; i++)
    for (j = 0; j <= N; j++)
      Z[i][j] = Z[j+N][i+N+1];

  for (i = 0; i <= N; i++)
    for (j = 0; j <=N; j++)
      if (Z[i][j] != B[j+N][i+N+1])
	abort();
}

int main(void)
{
  foo();

  return 0;
}

/* Check that parallel code generation part make the right answer.  */
/* { dg-final { scan-tree-dump-times "4 loops carried no dependency" 1 "graphite" } } */
/* { dg-final { scan-tree-dump-times "loopfn.0" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "loopfn.1" 4 "optimized" } } */
