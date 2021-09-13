/* { dg-do run } */

#include <omp.h>
#include <assert.h>

/* Test the task detach clause used together with dependencies.  */

int main (void)
{
  omp_event_handle_t detach_event;
  int x = 0, y = 0, z = 0;
  int dep;

  #pragma omp parallel
    #pragma omp single
    {
      #pragma omp task depend (out:dep) detach (detach_event)
	x++;

      #pragma omp task
      {
	y++;
	omp_fulfill_event (detach_event);
      }

      #pragma omp task depend (in:dep)
	z++;
    }

  assert (x == 1);
  assert (y == 1);
  assert (z == 1);
}
