#define N 500

int X[2*N], Y[2*N], B[2*N];
int A[2*N][2*N], C[2*N][2*N];

int foo(void)
{
  int i, j, k;

  for (i = 1; i <= N; i++)
    {
      X[i] = Y[i] + 10;
      for (j = 1; j <= N; j++)
	{
	  B[j] = A[j][N];
	  for (k = 1; k <= N; k++)
	    {
	      A[j+1][k] = B[j] + C[j][k];
	    }
	  Y[i+j] = A[j+1][N];
	}
    }

  return A[1][5]*B[6];
}

int main(void)
{
  foo();

  return 0;
}

/* Check that parallel code generation part make the right answer.  */
/* { dg-final { scan-tree-dump-times "1 loops carried no dependency" 1 "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
/* { dg-final { scan-tree-dump-times "loopfn.0" 5 "optimized" } } */
/* { dg-final { cleanup-tree-dump "parloops" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
