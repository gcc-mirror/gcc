// { dg-do run }
// { dg-set-target-env-var OMP_CANCELLATION "true" }

#include <omp.h>
#include "cancel-test.h"

__attribute__((noinline, noclone)) int
foo (int *x)
{
  S a, b, c, d, e;
  int v = 0, w = 0;
  #pragma omp parallel num_threads (32) shared (v, w) private (c, d) firstprivate (e)
  {
    S g;
    int i;
    c.bump ();
    e.bump ();
    #pragma omp for private (d, g) firstprivate (b)
    for (i = 0; i < 1000; ++i)
      {
	b.bump ();
	d.bump ();
	g.bump ();
	#pragma omp cancel for if (x[0])
	abort ();
      }
    #pragma omp for private (d, g) firstprivate (b)
    for (i = 0; i < 1000; ++i)
      {
	b.bump ();
	d.bump ();
	g.bump ();
	#pragma omp cancel for if (x[1])
	#pragma omp atomic
	v++;
      }
    #pragma omp for private (d, g) firstprivate (b)
    for (i = 0; i < 1000; ++i)
      {
	b.bump ();
	d.bump ();
	g.bump ();
	#pragma omp cancel for if (x[2])
	#pragma omp atomic
	w += 8;
      }
    #pragma omp for private (d, g) firstprivate (b)
    for (i = 0; i < 1000; ++i)
      {
	b.bump ();
	d.bump ();
	g.bump ();
	#pragma omp cancel for if (x[3])
	#pragma omp atomic
	v += 2;
      }
  }
  if (v != 3000 || w != 0)
    abort ();
  #pragma omp parallel num_threads (32) shared (v, w) private (c, d) firstprivate (e)
  {
    S g, h;
    int i;
    c.bump ();
    e.bump ();
    /* None of these cancel directives should actually cancel anything,
       but the compiler shouldn't know that and thus should use cancellable
       barriers at the end of all the workshares.  */
    #pragma omp cancel parallel if (omp_get_thread_num () == 1 && x[4])
    #pragma omp for private (d, g) firstprivate (b)
    for (i = 0; i < 1000; ++i)
      {
	b.bump ();
	d.bump ();
	g.bump ();
	#pragma omp cancel for if (x[0])
	abort ();
      }
    #pragma omp cancel parallel if (omp_get_thread_num () == 2 && x[4])
    #pragma omp for private (d, g) firstprivate (b)
    for (i = 0; i < 1000; ++i)
      {
	b.bump ();
	d.bump ();
	g.bump ();
	#pragma omp cancel for if (x[1])
	#pragma omp atomic
	v++;
      }
    #pragma omp cancel parallel if (omp_get_thread_num () == 3 && x[4])
    #pragma omp for private (d, g) firstprivate (b)
    for (i = 0; i < 1000; ++i)
      {
	b.bump ();
	d.bump ();
	g.bump ();
	#pragma omp cancel for if (x[2])
	#pragma omp atomic
	w += 8;
      }
    #pragma omp cancel parallel if (omp_get_thread_num () == 4 && x[4])
    #pragma omp for private (d, g) firstprivate (b)
    for (i = 0; i < 1000; ++i)
      {
	b.bump ();
	d.bump ();
	g.bump ();
	#pragma omp cancel for if (x[3])
	#pragma omp atomic
	v += 2;
      }
    #pragma omp cancel parallel if (omp_get_thread_num () == 5 && x[4])
  }
  if (v != 6000 || w != 0)
    abort ();
  return 0;
}

int
main ()
{
  int x[] = { 1, 0, 1, 0, 0 };
  if (omp_get_cancellation ())
    foo (x);
  S::verify ();
}
