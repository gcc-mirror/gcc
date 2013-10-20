// { dg-do run }
// { dg-set-target-env-var OMP_CANCELLATION "true" }

#include <unistd.h>
#include <omp.h>
#include "cancel-test.h"

void
foo ()
{
  S a, b, c, d, e, f;
  #pragma omp parallel private (c, d) firstprivate (e, f)
  #pragma omp taskgroup
  {
    c.bump ();
    e.bump ();
    #pragma omp task firstprivate (b, f) private (d)
    {
      S h;
      b.bump ();
      d.bump ();
      f.bump ();
      #pragma omp cancel taskgroup
      if (omp_get_cancellation ())
	abort ();
    }
  }
  #pragma omp parallel private (c, d) firstprivate (e, f)
  {
    #pragma omp barrier
    #pragma omp single
    #pragma omp taskgroup
    {
      int i;
      c.bump ();
      e.bump ();
      for (i = 0; i < 50; i++)
	#pragma omp task firstprivate (b, f) private (d)
	{
	  S h;
	  b.bump ();
	  d.bump ();
	  f.bump ();
	  #pragma omp cancellation point taskgroup
	  usleep (30);
	  #pragma omp cancel taskgroup if (i > 5)
	}
    }
    usleep (10);
  }
}

int
main ()
{
  foo ();
  S::verify ();
}
