/* PR middle-end/108459 */

char a[17][17];

__attribute__((noipa)) void
foo (int x, int y)
{
  #pragma omp for collapse(2)
  for (int i = 1; i <= 16; i++)
    for (int j = i * x + y; j <= 16; j++)
      a[i][j] = 1;
}

int
main ()
{
  #pragma omp parallel
  foo (1, 1);
  for (int i = 0; i <= 16; i++)
    for (int j = 0; j <= 16; j++)
      if (i >= 1 && j >= i + 1)
	{
	  if (a[i][j] != 1)
	    __builtin_abort ();
	  a[i][j] = 0;
	}
      else if (a[i][j])
	__builtin_abort ();
  #pragma omp parallel
  foo (2, -2);
  for (int i = 0; i <= 16; i++)
    for (int j = 0; j <= 16; j++)
      if (i >= 1 && j >= 2 * i - 2)
	{
	  if (a[i][j] != 1)
	    __builtin_abort ();
	}
      else if (a[i][j])
	__builtin_abort ();
  return 0;
}
