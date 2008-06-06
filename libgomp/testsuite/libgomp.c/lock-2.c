#include <omp.h>
#include <stdlib.h>

int
main (void)
{
  int l = 0;
  omp_nest_lock_t lock;
  omp_init_nest_lock (&lock);
#pragma omp parallel reduction (+:l) num_threads (1)
  {
    if (omp_test_nest_lock (&lock) != 1)
      l++;
    if (omp_test_nest_lock (&lock) != 2)
      l++;
  #pragma omp task if (0) shared (lock, l)
    {
      if (omp_test_nest_lock (&lock) != 0)
	l++;
    }
  #pragma omp taskwait
    if (omp_test_nest_lock (&lock) != 3)
      l++;
    omp_unset_nest_lock (&lock);
    omp_unset_nest_lock (&lock);
    omp_unset_nest_lock (&lock);
  }
  if (l)
    abort ();
  omp_destroy_nest_lock (&lock);
  return 0;
}
