void abort (void);

#define N 500
#define M 50

int X[2*N], Y[2*N], B[2*N];
int A[2*N][2*N], C[2*N][2*N];

static void __attribute__((noinline,noclone))
init (void)
{
  volatile int i, j;

  for (i = 0; i < 2 * N; ++i)
    {
      B[i] = 1;
      X[i] = 1;
      Y[i] = 1;
      for (j = 0; j < 2 * N; ++j)
	{
	  A[i][j] = 1;
	  C[i][j] = 1;
	}
    }
}

static void __attribute__((noinline,noclone))
foo (void)
{
  int i, j, k;

  for (i = 0; i < M; i++)
    {
      X[i] = Y[i] + 10;
      for (j = 0; j < M; j++)
	{
	  B[j] = A[j][N];
	  for (k = 0; k < N; k++)
	    {
	      A[j+1][k] = B[j] + C[j][k];
	    }
	  Y[i+j] = A[j+1][N];
	}
    }
}

static void __attribute__((noinline,noclone))
check (void)
{
  volatile int i, j;

  for (i = 0; i < 2 * N; ++i)
    {
      int expect_x = i < M ? 11 : 1;

      if (B[i] != 1
	  || X[i] != expect_x
	  || Y[i] != 1)
	abort ();

      for (j = 0; j < 2 * N; ++j)
	{
	  int expect_a = (0 < i && i <= M && j < N) ? 2 : 1;

	  if (A[i][j] != expect_a
	      || C[i][j] != 1)
	    abort ();
	}
    }
}

int main(void)
{
  init ();
  foo ();
  check ();

  return 0;
}

/* Check that parallel code generation part make the right answer.  */
/* { dg-final { scan-tree-dump-times "1 loops carried no dependency" 1 "graphite" } } */
/* { dg-final { scan-tree-dump-times "loopfn.0" 4 "optimized" } } */
