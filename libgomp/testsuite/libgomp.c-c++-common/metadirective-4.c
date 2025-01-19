/* { dg-do run } */

#include <omp.h>

#define N 100

int
f (int a[], int run_parallel, int run_static)
{
  int is_parallel = 0;
  int is_static = 0;

  #pragma omp metadirective \
	when (user={condition(run_parallel)}: parallel)
  {
    int i;

    if (omp_in_parallel ())
      is_parallel = 1;

    #pragma omp metadirective \
	when (construct={parallel}, user={condition(!run_static)}: \
	      for schedule(guided) private(is_static)) \
	when (construct={parallel}: for schedule(static))
      for (i = 0; i < N; i++)
	{
	  a[i] = i;
	  is_static = 1;
	}
   }

  return (is_parallel << 1) | is_static;
}

int
main (void)
{
  int a[N];

  /* is_static is always set if run_parallel is false.  */
  if (f (a, 0, 0) != 1)
    return 1;

  if (f (a, 0, 1) != 1)
    return 1;

  if (f (a, 1, 0) != 2)
    return 1;

  if (f (a, 1, 1) != 3)
    return 1;

  return 0;
}
