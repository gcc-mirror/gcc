/* Exercise nested function decomposition, gcc/tree-nested.c.  */

int
main (void)
{
  int p1 = 2, p2 = 6, p3 = 0, p4 = 4, p5 = 13, p6 = 18, p7 = 1, p8 = 1, p9 = 1;

  void test1 ()
  {
    int i, j, k;
    int a[4][4][4];

    __builtin_memset (a, '\0', sizeof (a));

#pragma acc parallel
#pragma acc loop collapse(3)
    for (i = 1; i <= 3; i++)
      for (j = 1; j <= 3; j++)
	for (k = 2; k <= 3; k++)
	  a[i][j][k] = 1;

    for (i = 1; i <= 3; i++)
      for (j = 1; j <= 3; j++)
	for (k = 2; k <= 3; k++)
	  if (a[i][j][k] != 1)
	    __builtin_abort();
  }

  void test2 (int v1, int v2, int v3, int v4, int v5, int v6)
  {
    int i, j, k, l = 0, r = 0;
    int a[7][5][19];
    int b[7][5][19];

    __builtin_memset (a, '\0', sizeof (a));
    __builtin_memset (b, '\0', sizeof (b));

#pragma acc parallel reduction (||:l)
#pragma acc loop reduction (||:l) collapse(3)
    for (i = v1; i <= v2; i++)
      for (j = v3; j <= v4; j++)
	for (k = v5; k <= v6; k++)
	  {
	    l = l || i < 2 || i > 6 || j < 0 || j > 4 || k < 13 || k > 18;
	    if (!l)
	      a[i][j][k] += 1;
	  }

    for (i = v1; i <= v2; i++)
      for (j = v3; j <= v4; j++)
	for (k = v5; k <= v6; k++)
	  {
	    r = r || i < 2 || i > 6 || j < 0 || j > 4 || k < 13 || k > 18;
	    if (!r)
	      b[i][j][k] += 1;
	  }

    if (l != r)
      __builtin_abort ();

    for (i = v1; i <= v2; i++)
      for (j = v3; j <= v4; j++)
	for (k = v5; k <= v6; k++)
	  if (b[i][j][k] != a[i][j][k])
	    __builtin_abort ();
  }

  void test3 (int v1, int v2, int v3, int v4, int v5, int v6, int v7, int v8,
      int v9)
  {
    int i, j, k, l = 0, r = 0;
    int a[7][5][19];
    int b[7][5][19];

    __builtin_memset (a, '\0', sizeof (a));
    __builtin_memset (b, '\0', sizeof (b));

#pragma acc parallel reduction (||:l)
#pragma acc loop reduction (||:l) collapse(3)
    for (i = v1; i <= v2; i += v7)
      for (j = v3; j <= v4; j += v8)
	for (k = v5; k <= v6; k += v9)
	  {
	    l = l || i < 2 || i > 6 || j < 0 || j > 4 || k < 13 || k > 18;
	    if (!l)
	      a[i][j][k] += 1;
	  }

    for (i = v1; i <= v2; i += v7)
      for (j = v3; j <= v4; j += v8)
	for (k = v5; k <= v6; k += v9)
	  {
	    r = r || i < 2 || i > 6 || j < 0 || j > 4 || k < 13 || k > 18;
	    if (!r)
	      b[i][j][k] += 1;
	  }

    if (l != r)
      __builtin_abort ();

    for (i = v1; i <= v2; i++)
      for (j = v3; j <= v4; j++)
	for (k = v5; k <= v6; k++)
	  if (b[i][j][k] != a[i][j][k])
	    __builtin_abort ();
  }

  void test4 ()
  {
    int i, j, k, l = 0, r = 0;
    int a[7][5][19];
    int b[7][5][19];
    int v1 = p1, v2 = p2, v3 = p3, v4 = p4, v5 = p5, v6 = p6, v7 = p7, v8 = p8,
      v9 = p9;

    __builtin_memset (a, '\0', sizeof (a));
    __builtin_memset (b, '\0', sizeof (b));

#pragma acc parallel reduction (||:l)
#pragma acc loop reduction (||:l) collapse(3)
    for (i = v1; i <= v2; i += v7)
      for (j = v3; j <= v4; j += v8)
	for (k = v5; k <= v6; k += v9)
	  {
	    l = l || i < 2 || i > 6 || j < 0 || j > 4 || k < 13 || k > 18;
	    if (!l)
	      a[i][j][k] += 1;
	  }

    for (i = v1; i <= v2; i += v7)
      for (j = v3; j <= v4; j += v8)
	for (k = v5; k <= v6; k += v9)
	  {
	    r = r || i < 2 || i > 6 || j < 0 || j > 4 || k < 13 || k > 18;
	    if (!r)
	      b[i][j][k] += 1;
	  }

    if (l != r)
      __builtin_abort ();

    for (i = v1; i <= v2; i++)
      for (j = v3; j <= v4; j++)
	for (k = v5; k <= v6; k++)
	  if (b[i][j][k] != a[i][j][k])
	    __builtin_abort ();
  }

  test1 ();
  test2 (p1, p2, p3, p4, p5, p6);
  test3 (p1, p2, p3, p4, p5, p6, p7, p8, p9);
  test4 ();

  return 0;
}
