/* { dg-do run } */

#include <omp.h>
#include <stdlib.h>

void
foo_host (void)
{
  if (!omp_is_initial_device ())
    abort ();
}

#pragma omp declare variant (foo_host) match (device={kind(host)})
void
foo (void)
{
  if (omp_is_initial_device ())
    abort ();
}

void
bar_nohost (void)
{
  if (omp_is_initial_device ())
    abort ();
}

#pragma omp declare variant (bar_nohost) match (device={kind(nohost)})
void
bar (void)
{
  if (!omp_is_initial_device ())
    abort ();
}

int
main ()
{
  #pragma omp target
  {
    foo ();
    bar ();
  }
  return 0;
}
