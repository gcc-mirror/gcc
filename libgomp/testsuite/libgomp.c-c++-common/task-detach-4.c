/* { dg-do run } */

#include <omp.h>
#include <assert.h>

/* Test detach clause, where a task fulfills its own completion event.  */

int main (void)
{
  omp_event_handle_t detach_event;
  int x = 0;

  detach_event = (omp_event_handle_t) 0x123456789abcdef0;

  #pragma omp parallel
    #pragma omp single
      #pragma omp task detach (detach_event)
      {
	x++;
	omp_fulfill_event (detach_event);
      }

  assert (x == 1);
}
