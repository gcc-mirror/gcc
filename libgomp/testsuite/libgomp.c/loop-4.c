/* { dg-do run } */

extern void abort (void);

int
main (void)
{
  int e = 0;
#pragma omp parallel num_threads (4) reduction(+:e)
  {
    long i;
    #pragma omp for schedule(dynamic,1)
    for (i = __LONG_MAX__ - 30001; i <= __LONG_MAX__ - 10001; i += 10000)
      if (i != __LONG_MAX__ - 30001
	  && i != __LONG_MAX__ - 20001
	  && i != __LONG_MAX__ - 10001)
	e = 1;
    #pragma omp for schedule(dynamic,1)
    for (i = -__LONG_MAX__ + 30000; i >= -__LONG_MAX__ + 10000; i -= 10000)
      if (i != -__LONG_MAX__ + 30000
	  && i != -__LONG_MAX__ + 20000
	  && i != -__LONG_MAX__ + 10000)
	e = 1;
  }
  if (e)
    abort ();
  return 0;
}
