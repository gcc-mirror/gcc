/* { dg-do run } */

#ifndef __cplusplus
extern void abort (void);
#else
extern "C" void abort (void);
#endif

static int inner_loop_count = 0;
static int intervening_code_count = 0;

void
g (int x, int y)
{
  inner_loop_count++;
}

int
foo (int imax, int jmax)
{
  int j = 0;

#pragma omp for collapse(2)
  for (int i = 0; i < imax; ++i)
    {
      /* All the intervening code at the same level must be executed
	 the same number of times.  */
      ++intervening_code_count;
      for (int j = 0; j < jmax; ++j)
	{
	  g (i, j);
	}
      /* This is the outer j, not the one from the inner collapsed loop.  */
      ++j;
    }
  return j;
}

int
main (void)
{
  int j = foo (5, 3);
  if (j != intervening_code_count)
    abort ();
  if (inner_loop_count != 5 * 3)
    abort ();
  if (intervening_code_count < 5 || intervening_code_count > 5 * 3)
    abort ();
}
