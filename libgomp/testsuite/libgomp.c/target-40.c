#include <omp.h>
#include <stdlib.h>

int
main ()
{
  if (omp_get_initial_device () != omp_get_num_devices ())
    abort ();
  return 0;
}
