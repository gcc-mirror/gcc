/* PR libgomp/61200 */
/* { dg-do run } */

#include <omp.h>
#include <stdlib.h>
#include <unistd.h>

volatile int x;

void
foo ()
{
  int var = 1;
  int i;

  for (i = 0; i < 2; i++)
    {
      if (i == 1)
	{
	  #pragma omp parallel num_threads(2)
	    if (x)
	      var++;
	    else
	      {
		#pragma omp single
		  sleep (2);
	      }
	}
      else
	{
	  #pragma omp task shared(var)
	  {
	    sleep (1);
	    var = 2;
	  }
	}
    }
  #pragma omp taskwait
  if (var != 2)
    abort ();
}

void
bar ()
{
  int var = 1;
  int i;

  for (i = 0; i < 2; i++)
    {
      if (i == 0)
	{
	  #pragma omp task shared(var)
	  {
	    sleep (1);
	    var = 2;
	  }
	}
      else
	{
	  #pragma omp parallel num_threads(2)
	    if (x)
	      var++;
	    else
	      {
		#pragma omp single
		  sleep (2);
	      }
	}
    }
  #pragma omp taskwait
  if (var != 2)
    abort ();
}

int
main ()
{
  omp_set_nested (1);
  #pragma omp parallel num_threads(2)
    #pragma omp single
      foo ();
  #pragma omp parallel num_threads(2)
    #pragma omp single
      bar ();
  return 0;
}
