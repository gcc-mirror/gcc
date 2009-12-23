void foo(int *a, int *b)
{
  int i;
  int *c = b+1;

  for (i = 0; i < 100; i++)
    a[i] = c[i];
}

int main(void)
{
  return 0;
}

/* Check that parallel code generation part make the right answer.  */
/* { dg-final { cleanup-tree-dump "graphite" } } */
/* { dg-final { scan-tree-dump-times "loopfn" 0 "optimized" } } */
/* { dg-final { cleanup-tree-dump "parloops" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
