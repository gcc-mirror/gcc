int foo(int *a, int n)
{
  int i;
  for (i = 2; i < n; i++)
    a[i] += a[i+1];
}

/* Check that Graphite dependency checking notes the dependency.  */
/* { dg-do compile } */
/* { dg-final { scan-tree-dump-times "0 loops carried no dependency" 1 "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
/* { dg-final { cleanup-tree-dump "parloops" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
