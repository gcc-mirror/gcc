/* { dg-do run } */

/* Like imperfect1.c, but also includes loop transforms.  */

static int f1count[3], f2count[3];

int
f1 (int depth, int iter)
{
  f1count[depth]++;
  return iter;
}

int
f2 (int depth, int iter)
{
  f2count[depth]++;
  return iter;
}

void
s1 (int a1, int a2, int a3)
{
  int i, j, k;

  #pragma omp for collapse(2) private (j, k)
  for (i = 0; i < a1; i++)
    {
      f1 (0, i);
      for (j = 0; j < a2; j++)
	{
	  f1 (1, j);
	  #pragma omp tile sizes(5)
	  for (k = 0; k < a3; k++)
	    {
	      f1 (2, k);
	      f2 (2, k);
	    }
	  f2 (1, j);
	}
      f2 (0, i);
    }
}

int
main ()
{
  f1count[0] = 0;
  f1count[1] = 0;
  f1count[2] = 0;
  f2count[0] = 0;
  f2count[1] = 0;
  f2count[2] = 0;

  s1 (3, 4, 5);

  /* All intervening code at the same depth must be executed the same
     number of times. */
  if (f1count[0] != f2count[0]) __builtin_abort ();
  if (f1count[1] != f2count[1]) __builtin_abort ();
  if (f1count[2] != f2count[2]) __builtin_abort ();

  /* Intervening code must be executed at least as many times as the loop
     that encloses it. */
  if (f1count[0] < 3) __builtin_abort ();
  if (f1count[1] < 3 * 4) __builtin_abort ();

  /* Intervening code must not be executed more times than the number
     of logical iterations. */
  if (f1count[0] > 3 * 4 * 5) __builtin_abort ();
  if (f1count[1] > 3 * 4 * 5) __builtin_abort ();

  /* Check that the innermost loop body is executed exactly the number
     of logical iterations expected. */
  if (f1count[2] != 3 * 4 * 5) __builtin_abort ();
}
