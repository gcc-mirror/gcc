/* { dg-do run } */

#include <stdlib.h>
#include <unistd.h>

__attribute__((noinline, noclone, noreturn))
void
foo ()
{
  sleep (4);
  exit (0);
}

int
main ()
{
  #pragma omp parallel
  {
    #pragma omp sections
      {
        foo ();
      #pragma omp section
        foo ();
      #pragma omp section
        foo ();
      }
  }
  return 0;
}
