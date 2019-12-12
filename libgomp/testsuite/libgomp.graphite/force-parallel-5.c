/* Triangle loops.  */
void abort (void);

#define N 500

void foo(void)
{
  int i,j;
  int A[3*N], B[3*N];

  for (i = 0; i < 3*N; i++)
    B[i] = A[i] = i;

  for (i = 1; i < N; i++)
    for (j = 1; j < i; j++)
      /* This loop carried no dependency, it fails
	 at code generation part.*/
      A[j+N] = A[j] + j;

  for (i = 1; i < N; i++)
    for (j = 1; j < i; j++)
      if (A[j+N] != B[j] + j)
	abort();
}

int main(void)
{
  foo();

  return 0;
}

/* Check that parallel code generation part make the right answer.  */
/* { dg-final { scan-tree-dump-times "2 loops carried no dependency" 1 "graphite" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "loopfn.0" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "loopfn.1" 4 "optimized" } } */
