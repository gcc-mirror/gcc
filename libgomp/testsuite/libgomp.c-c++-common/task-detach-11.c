/* { dg-do run } */

#include <omp.h>

/* Test the detach clause when the task is undeferred.  */

int main (void)
{
  omp_event_handle_t event;

  #pragma omp task detach (event)
    omp_fulfill_event (event);
}
