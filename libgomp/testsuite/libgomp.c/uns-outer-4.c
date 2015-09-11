/* { dg-do run } */
/* { dg-additional-options "-ftree-parallelize-loops=2" } */

void abort (void);

unsigned int g_sum = 1;

unsigned int x[500][500];

void __attribute__((noinline,noclone))
parloop (int N)
{
  int i, j;
  unsigned int sum;

  /* Double reduction is detected, outer loop is parallelized.  */
  sum = 0;
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      sum += x[i][j];

  g_sum = sum;
}

int
main (void)
{
  x[234][432] = 2;

  parloop (500);

  if (g_sum != 2)
    abort ();

  return 0;
}
