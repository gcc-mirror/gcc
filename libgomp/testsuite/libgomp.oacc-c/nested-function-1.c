/* Exercise nested function decomposition, gcc/tree-nested.c.  */

int
main (void)
{
  void test1 ()
  {
    int i, j, k;
    int a[4][7][8];

    __builtin_memset (a, 0, sizeof (a));

#pragma acc parallel
#pragma acc loop collapse(4 - 1)
    for (i = 1; i <= 3; i++)
      for (j = 4; j <= 6; j++)
	for (k = 5; k <= 7; k++)
	  a[i][j][k] = i + j + k;

    for (i = 1; i <= 3; i++)
      for (j = 4; j <= 6; j++)
	for (k = 5; k <= 7; k++)
	  if (a[i][j][k] != i + j + k)
	    __builtin_abort();
  }

  void test2 ()
  {
    int i, j, k;
    int a[4][4][4];

    __builtin_memset (a, 0, sizeof (a));

#pragma acc parallel
#pragma acc loop collapse(3)
    for (i = 1; i <= 3; i++)
      for (j = 1; j <= 3; j++)
	for (k = 1; k <= 3; k++)
	  a[i][j][k] = 1;

    for (i = 1; i <= 3; i++)
      for (j = 1; j <= 3; j++)
	for (k = 1; k <= 3; k++)
	  if (a[i][j][k] != 1)
	    __builtin_abort ();
  }

  test1 ();
  test2 ();

  return 0;
}
