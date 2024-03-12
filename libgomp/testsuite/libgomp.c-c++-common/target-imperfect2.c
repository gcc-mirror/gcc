/* { dg-do run } */

/* Like imperfect2.c, but enables offloading.  */

static int f1count[3], f2count[3];
static int g1count[3], g2count[3];
#pragma omp declare target enter (f1count, f2count)
#pragma omp declare target enter (g1count, g2count)

#ifndef __cplusplus
extern void abort (void);
#else
extern "C" void abort (void);
#endif

int f1 (int depth, int iter)
{
  #pragma omp atomic
  f1count[depth]++;
  return iter;
}

int f2 (int depth, int iter)
{
  #pragma omp atomic
  f2count[depth]++;
  return iter;
}

int g1 (int depth, int iter)
{
  #pragma omp atomic
  g1count[depth]++;
  return iter;
}

int g2 (int depth, int iter)
{
  #pragma omp atomic
  g2count[depth]++;
  return iter;
}

void s1 (int a1, int a2, int a3)
{
  int i, j, k;

#pragma omp target parallel for collapse(3) map(always, tofrom:f1count, f2count, g1count, g2count)
  for (i = 0; i < a1; i++)
    {
      f1 (0, i);
      {
	g1 (0, i);
	for (j = 0; j < a2; j++)
	  {
	    f1 (1, j);
	    {
	      g1 (1, j);
	      for (k = 0; k < a3; k++)
		{
		  f1 (2, k);
		  {
		    g1 (2, k);
		    g2 (2, k);
		  }
		  f2 (2, k);
		}
	      g2 (1, j);
	    }
	  f2 (1, j);
	  }
	g2 (0, i);
      }
      f2 (0, i);
    }
}

int
main (void)
{
  f1count[0] = 0;
  f1count[1] = 0;
  f1count[2] = 0;
  f2count[0] = 0;
  f2count[1] = 0;
  f2count[2] = 0;

  g1count[0] = 0;
  g1count[1] = 0;
  g1count[2] = 0;
  g2count[0] = 0;
  g2count[1] = 0;
  g2count[2] = 0;

  s1 (3, 4, 5);

  /* All intervening code at the same depth must be executed the same
     number of times. */
  if (f1count[0] != f2count[0]) abort ();
  if (f1count[1] != f2count[1]) abort ();
  if (f1count[2] != f2count[2]) abort ();
  if (g1count[0] != f1count[0]) abort ();
  if (g2count[0] != f1count[0]) abort ();
  if (g1count[1] != f1count[1]) abort ();
  if (g2count[1] != f1count[1]) abort ();
  if (g1count[2] != f1count[2]) abort ();
  if (g2count[2] != f1count[2]) abort ();

  /* Intervening code must be executed at least as many times as the loop
     that encloses it. */
  if (f1count[0] < 3) abort ();
  if (f1count[1] < 3 * 4) abort ();

  /* Intervening code must not be executed more times than the number
     of logical iterations. */
  if (f1count[0] > 3 * 4 * 5) abort ();
  if (f1count[1] > 3 * 4 * 5) abort ();

  /* Check that the innermost loop body is executed exactly the number
     of logical iterations expected. */
  if (f1count[2] != 3 * 4 * 5) abort ();
}
