// { dg-do compile }

#include <omp.h>

int
main()
{
  omp_lock_t lock;
  omp_init_lock_with_hint(&lock, omp_lock_hint_none); // { dg-warning "'omp_lock_hint_none' is deprecated \\\[-Wdeprecated-declarations\\\]" }
  omp_destroy_lock(&lock);
  return 0;
}
