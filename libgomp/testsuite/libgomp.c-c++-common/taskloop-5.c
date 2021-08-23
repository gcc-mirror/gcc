/* { dg-do run } */
/* { dg-options "-O2" } */

int u[64], v, w[64];

__attribute__((noinline, noclone)) int
test (int a, int b, int c, int d, void (*fn) (int, int, int, int),
      int *num_tasks, int *min_iters, int *max_iters, int *sep)
{
  int i, j, t = 0;
  __builtin_memset (u, 0, sizeof u);
  v = 0;
  fn (a, b, c, d);
  *min_iters = 0;
  *max_iters = 0;
  *num_tasks = v;
  *sep = v;
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
      if (*min_iters != *max_iters)
	{
	  for (i = 0; i < v - 1; i++)
	    {
	      int min_idx = i;
	      for (j = i + 1; j < v; j++)
		if (w[min_idx] > w[j])
		  min_idx = j;
	      if (min_idx != i)
		{
		  int tem = u[i];
		  u[i] = u[min_idx];
		  u[min_idx] = tem;
		  tem = w[i];
		  w[i] = w[min_idx];
		  w[min_idx] = tem;
		}
	    }
	  if (u[0] != *max_iters)
	    __builtin_abort ();
	  for (i = 1; i < v; i++)
	    if (u[i] != u[i - 1])
	      {
		if (*sep != v || u[i] != *min_iters)
		  __builtin_abort ();
		*sep = i;
	      }
	}
    }
  return t;
}

void
grainsize (int a, int b, int c, int d)
{
  int i, j = 0, k = 0;
  #pragma omp taskloop firstprivate (j, k) grainsize(strict:d)
  for (i = a; i < b; i += c)
    {
      if (j == 0)
	{
	  #pragma omp atomic capture
	    k = v++;
	  if (k >= 64)
	    __builtin_abort ();
	  w[k] = i;
	}
      u[k] = ++j;
    }
}

void
num_tasks (int a, int b, int c, int d)
{
  int i, j = 0, k = 0;
  #pragma omp taskloop firstprivate (j, k) num_tasks(strict:d)
  for (i = a; i < b; i += c)
    {
      if (j == 0)
	{
	  #pragma omp atomic capture
	    k = v++;
	  if (k >= 64)
	    __builtin_abort ();
	  w[k] = i;
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
	int min_iters, max_iters, ntasks, sep;
	/* If grainsize is present and has strict modifier, # of task loop iters is == grainsize,
	   except that it can be smaller on the last task.  */
	if (test (0, 79, 1, 17, grainsize, &ntasks, &min_iters, &max_iters, &sep) != 79
	    || ntasks != 5 || min_iters != 11 || max_iters != 17 || sep != 4)
	  __builtin_abort ();
	if (test (-49, 2541, 7, 28, grainsize, &ntasks, &min_iters, &max_iters, &sep) != 370
	    || ntasks != 14 || min_iters != 6 || max_iters != 28 || sep != 13)
	  __builtin_abort ();
	if (test (7, 21, 2, 15, grainsize, &ntasks, &min_iters, &max_iters, &sep) != 7
	    || ntasks != 1 || min_iters != 7 || max_iters != 7 || sep != 1)
	  __builtin_abort ();
	/* If num_tasks is present, # of tasks is min (# of loop iters, num_tasks)
	   and each task has at least one iteration.  If strict modifier is present,
	   first set of tasks has ceil (# of loop iters / num_tasks) iterations,
	   followed by possibly empty set of tasks with floor (# of loop iters / num_tasks)
	   iterations.  */
	if (test (-51, 2500, 48, 9, num_tasks, &ntasks, &min_iters, &max_iters, &sep) != 54
	    || ntasks != 9 || min_iters != 6 || max_iters != 6 || sep != 9)
	  __builtin_abort ();
	if (test (0, 57, 1, 9, num_tasks, &ntasks, &min_iters, &max_iters, &sep) != 57
	    || ntasks != 9 || min_iters != 6 || max_iters != 7 || sep != 3)
	  __builtin_abort ();
	if (test (0, 25, 2, 17, num_tasks, &ntasks, &min_iters, &max_iters, &sep) != 13
	    || ntasks != 13 || min_iters != 1 || max_iters != 1 || sep != 13)
	  __builtin_abort ();
      }
  return 0;
}
