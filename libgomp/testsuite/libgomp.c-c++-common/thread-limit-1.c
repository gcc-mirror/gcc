#include <omp.h>
#include <stdlib.h>

void
foo ()
{
  {
    #pragma omp target parallel nowait thread_limit (4) num_threads (1)
    if (omp_get_thread_limit () > 4)
      abort ();
  }
  #pragma omp taskwait
}

int
main ()
{
  #pragma omp target thread_limit (6)
  if (omp_get_thread_limit () > 6)
    abort ();
  foo ();
  return 0;
}
