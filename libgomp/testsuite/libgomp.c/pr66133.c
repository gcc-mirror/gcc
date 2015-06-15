/* PR middle-end/66133 */
/* { dg-do run } */

#include <stdlib.h>
#include <unistd.h>

volatile int x;

__attribute__((noinline)) void
foo (void)
{
  if (x == 0)
    {
      #pragma omp task
	{
	  usleep (2000);
	  exit (0);
	}
    }
  else
    abort ();
}

int
main ()
{
  #pragma omp parallel num_threads (2)
    {
      #pragma omp barrier
      #pragma omp single
	foo ();
    }
  exit (0);
}
