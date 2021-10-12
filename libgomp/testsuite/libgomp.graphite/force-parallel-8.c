/* { dg-additional-options "-fdisable-tree-thread1 -fdisable-tree-vrp-thread1 --param max-stores-to-sink=0" } */

#define N 1500

int x[N][N], y[N];

void abort (void);

int foo(void)
{
  int i, j;

  for (i = 0; i < N; i++)
    y[i] = i;

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      x[i][j] = i + j;

  for (i = 0; i < N; i++)
    {
      y[i] = i;

      for (j = 0; j < N; j++)
	{
	  if (j > 500)
	    {
	      x[i][j] = i + j + 3;
	      y[j] = i*j + 10;
	    }
	  else
	    x[i][j] = x[i][j]*3;
	}
    }

  return x[2][5]*y[8];
}

int main(void)
{
  if (168 != foo())
    abort ();

  return 0;
}

/* Check that parallel code generation part make the right answer.  */
/* { dg-final { scan-tree-dump-times "5 loops carried no dependency" 1 "graphite" } } */
/* { dg-final { scan-tree-dump-times "loopfn.0" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "loopfn.1" 4 "optimized" } } */
