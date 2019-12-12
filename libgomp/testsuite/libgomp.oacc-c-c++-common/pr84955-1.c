/* { dg-do compile }  */
/* { dg-options "-O2 -fdump-tree-cddce2 -ffinite-loops" } */

int
f1 (void)
{
  int i, j;

#pragma acc parallel loop tile(2,3)
  for (i = 1; i < 10; i++)
    for (j = 1; j < 10; j++)
      for (;;)
	;

  return i + j;
}

int
f2 (void)
{
  int i, j, k;

#pragma acc parallel loop tile(2,3)
  for (i = 1; i < 10; i++)
    for (j = 1; j < 10; j++)
      for (k = 1; k < 10; k++)
	;

  return i + j;
}
/* { dg-final { scan-tree-dump-not "if" "cddce2"} } */
