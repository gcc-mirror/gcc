#include <stdlib.h>
#include <omp.h>

int
main ()
{
  #pragma omp teams thread_limit (1)
  if (omp_get_thread_limit () != 1)
    abort ();
  return 0;
}
