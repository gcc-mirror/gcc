// { dg-do run }

#include <omp.h>
extern "C" void abort ();

int l = 5;

int
foo (int i)
{
  int j = 7;
  const int k = 8;
  #pragma omp task firstprivate (i) shared (j, l)
  {
    #pragma omp critical
      {
	j += i;
	l += k;
      }
  }
  i++;
  #pragma omp task firstprivate (i) shared (j, l)
  {
    #pragma omp critical
      {
	j += i;
	l += k;
      }
  }
  i++;
  #pragma omp task firstprivate (i) shared (j, l)
  {
    #pragma omp critical
      {
	j += i;
	l += k;
      }
  }
  i++;
  #pragma omp task firstprivate (i) shared (j, l)
  {
    #pragma omp critical
      {
	j += i;
	l += k;
      }
  }
  i++;
  #pragma omp taskwait
  return (i != 8 * omp_get_thread_num () + 4
	  || j != 4 * i - 3
	  || k != 8);
}

int
main (void)
{
  int r = 0;
  #pragma omp parallel num_threads (4) reduction(+:r)
    if (omp_get_num_threads () != 4)
      {
	#pragma omp master
	  l = 133;
      }
    else if (foo (8 * omp_get_thread_num ()))
      r++;
  if (r || l != 133)
    abort ();
  return 0;
}
