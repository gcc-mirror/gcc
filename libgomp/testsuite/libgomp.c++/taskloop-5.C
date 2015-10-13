#include <omp.h>

__attribute__((noinline, noclone)) void
foo (int &b)
{
#pragma omp parallel
#pragma omp single
  {
    bool f = false;
  #pragma omp taskloop firstprivate (b, f)
    for (int i = 0; i < 30; i++)
      {
	int q = omp_get_thread_num ();
	if (!f)
	  {
	    if (b != 2)
	      __builtin_abort ();
	  }
	else if (b != 8 * q)
	  __builtin_abort ();
	b = 8 * q;
	f = true;
      }
  }
  int n;
#pragma omp parallel
#pragma omp single
  {
    bool f = false;
  #pragma omp taskloop firstprivate (f) lastprivate (b, n)
    for (int i = 0; i < 30; i++)
      {
	int q = omp_get_thread_num ();
	if (f && b != 8 * q)
	  __builtin_abort ();
	b = 8 * q;
	n = q;
	f = true;
      }
  }
  if (b != 8 * n)
    __builtin_abort ();
  b = 9;
#pragma omp parallel
#pragma omp single
  {
    bool f = false;
  #pragma omp taskloop firstprivate (b, f) lastprivate (b, n)
    for (int i = 0; i < 30; i++)
      {
	int q = omp_get_thread_num ();
	if (!f)
	  {
	    if (b != 9)
	      __builtin_abort ();
	  }
	else if (b != 11 * q)
	  __builtin_abort ();
	b = 11 * q;
	n = q;
	f = true;
      }
  }
  if (b != 11 * n)
    __builtin_abort ();
}

int
main ()
{
  int b = 2;
  foo (b);
}
