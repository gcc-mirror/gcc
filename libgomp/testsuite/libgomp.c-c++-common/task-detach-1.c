/* { dg-do run } */

#include <omp.h>
#include <assert.h>

/* Test chaining of detached tasks, with each task fulfilling the
   completion event of the previous one.  */

int main (void)
{
  omp_event_handle_t detach_event1, detach_event2;
  int x = 0, y = 0, z = 0;

  #pragma omp parallel
    #pragma omp single
    {
      #pragma omp task detach (detach_event1)
	x++;

      #pragma omp task detach (detach_event2)
      {
	y++;
	omp_fulfill_event (detach_event1);
      }

      #pragma omp task
      {
	z++;
	omp_fulfill_event (detach_event2);
      }
    }

  assert (x == 1);
  assert (y == 1);
  assert (z == 1);
}
