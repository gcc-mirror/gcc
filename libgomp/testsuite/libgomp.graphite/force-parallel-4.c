/* Autopar with IF conditions.  */
/* { dg-additional-options "-fno-thread-jumps" } */

void abort();

#define N 10000
#define T 1000

void foo(void)
{
  int i;
  int A[2*N], B[2*N];

  /* Initialize array: carried no dependency.  */
  for (i = 0; i < 2*N; i++)
    B[i] = A[i] = i;

  for (i = 0; i < N; i++)
    {
      if (i < T)
	/* loop i1: carried no dependency.  */
	A[i] = A[i+T];
      else
	/* loop i2: carried dependency.  */
	A[i] = A[i+T+1];
    }

  /* If it runs a wrong answer, abort.  */
  for (i = 0; i < N; i++)
    {
      if (i < T)
	{
	  if (A[i] != B[i+T])
	    abort();
	}
      else
	{
	  if (A[i] != B[i+T+1])
	    abort();
	}
    }
}

int main(void)
{
  foo();
  return 0;
}

/* Check that parallel code generation part make the right answer.
   ???  XFAILed for i1 because conditional store elimination wrecks
   our dependence representation.  */
/* { dg-final { scan-tree-dump-times "2 loops carried no dependency" 1 "graphite" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "1 loops carried no dependency" 1 "graphite" } } */
/* { dg-final { scan-tree-dump-times "loopfn.0" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "loopfn.1" 4 "optimized" } } */
