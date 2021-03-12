/* { dg-do run } */

#include <omp.h>
#include <assert.h>

/* Test handling of detach clause with only a single thread.  The runtime
   should not block when a task with an unfulfilled event finishes
   running.  */

int main (void)
{
  omp_event_handle_t detach_event1, detach_event2;
  int x = 0, y = 0, z = 0;

  #pragma omp parallel num_threads (1)
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
