/* PR middle-end/39154 */
/* { dg-do compile } */
/* { dg-options "-O2 -std=gnu99" } */

extern void abort (void);

int n = 20;

int
main (void)
{
  int a[n], b[n][n];

#pragma omp parallel for
    for (int i = 0; i < n; i++)
      {
	a[i] = i + 1;
#pragma omp parallel for
	for (int j = 0; j < n; j++)
	  b[i][j] = a[i];
      }

  for (int i = 0; i < n; i++)
    {
      for (int j = 0; j < n; j++)
	if (b[i][j] != i + 1)
	  abort ();
      if (a[i] != i + 1)
	abort ();
    }

#pragma omp parallel for shared (n, a, b)
    for (int i = 0; i < n; i++)
      {
	a[i] = i + 3;
#pragma omp parallel for
	for (int j = 0; j < n; j++)
	  b[i][j] = a[i];
      }

  for (int i = 0; i < n; i++)
    {
      for (int j = 0; j < n; j++)
	if (b[i][j] != i + 3)
	  abort ();
      if (a[i] != i + 3)
	abort ();
    }

#pragma omp parallel for
    for (int i = 0; i < n; i++)
      {
	a[i] = i + 5;
#pragma omp parallel for shared (n, a, b)
	for (int j = 0; j < n; j++)
	  b[i][j] = a[i];
      }

  for (int i = 0; i < n; i++)
    {
      for (int j = 0; j < n; j++)
	if (b[i][j] != i + 5)
	  abort ();
      if (a[i] != i + 5)
	abort ();
    }

#pragma omp parallel for shared (n, a, b)
    for (int i = 0; i < n; i++)
      {
	a[i] = i + 7;
#pragma omp parallel for shared (n, a, b)
	for (int j = 0; j < n; j++)
	  b[i][j] = a[i];
      }

  for (int i = 0; i < n; i++)
    {
      for (int j = 0; j < n; j++)
	if (b[i][j] != i + 7)
	  abort ();
      if (a[i] != i + 7)
	abort ();
    }

#pragma omp parallel for private (a, b)
    for (int i = 0; i < n; i++)
      {
	a[i] = i + 1;
#pragma omp parallel for
	for (int j = 0; j < n; j++)
	  b[i][j] = a[i];
      }

#pragma omp parallel for private (a, b)
    for (int i = 0; i < n; i++)
      {
	a[i] = i + 1;
#pragma omp parallel for private (b)
	for (int j = 0; j < n; j++)
	  b[i][j] = a[i];
      }

  return 0;
}
