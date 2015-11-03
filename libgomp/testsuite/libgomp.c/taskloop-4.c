/* { dg-do run } */
/* { dg-options "-O2 -fopenmp" } */

int u[64], v;

__attribute__((noinline, noclone)) int
test (int a, int b, int c, int d, void (*fn) (int, int, int, int),
      int *num_tasks, int *min_iters, int *max_iters)
{
  int i, t = 0;
  __builtin_memset (u, 0, sizeof u);
  v = 0;
  fn (a, b, c, d);
  *min_iters = 0;
  *max_iters = 0;
  *num_tasks = v;
  if (v)
    {
      *min_iters = u[0];
      *max_iters = u[0];
      t = u[0];
      for (i = 1; i < v; i++)
	{
	  if (*min_iters > u[i])
	    *min_iters = u[i];
	  if (*max_iters < u[i])
	    *max_iters = u[i];
	  t += u[i];
	}
    }
  return t;
}

void
grainsize (int a, int b, int c, int d)
{
  int i, j = 0, k = 0;
  #pragma omp taskloop firstprivate (j, k) grainsize(d)
  for (i = a; i < b; i += c)
    {
      if (j == 0)
	{
	  #pragma omp atomic capture
	    k = v++;
	  if (k >= 64)
	    __builtin_abort ();
	}
      u[k] = ++j;
    }
}

void
num_tasks (int a, int b, int c, int d)
{
  int i, j = 0, k = 0;
  #pragma omp taskloop firstprivate (j, k) num_tasks(d)
  for (i = a; i < b; i += c)
    {
      if (j == 0)
	{
	  #pragma omp atomic capture
	    k = v++;
	  if (k >= 64)
	    __builtin_abort ();
	}
      u[k] = ++j;
    }
}

int
main ()
{
  #pragma omp parallel
    #pragma omp single
      {
	int min_iters, max_iters, ntasks;
	/* If grainsize is present, # of task loop iters is >= grainsize && < 2 * grainsize,
	   unless # of loop iterations is smaller than grainsize.  */
	if (test (0, 79, 1, 17, grainsize, &ntasks, &min_iters, &max_iters) != 79
	    || min_iters < 17 || max_iters >= 17 * 2)
	  __builtin_abort ();
	if (test (-49, 2541, 7, 28, grainsize, &ntasks, &min_iters, &max_iters) != 370
	    || min_iters < 28 || max_iters >= 28 * 2)
	  __builtin_abort ();
	if (test (7, 21, 2, 15, grainsize, &ntasks, &min_iters, &max_iters) != 7
	    || ntasks != 1 || min_iters != 7 || max_iters != 7)
	  __builtin_abort ();
	/* If num_tasks is present, # of task loop iters is min (# of loop iters, num_tasks).  */
	if (test (-51, 2500, 48, 9, num_tasks, &ntasks, &min_iters, &max_iters) != 54
	    || ntasks != 9)
	  __builtin_abort ();
	if (test (0, 25, 2, 17, num_tasks, &ntasks, &min_iters, &max_iters) != 13
	    || ntasks != 13)
	  __builtin_abort ();
      }
  return 0;
}
