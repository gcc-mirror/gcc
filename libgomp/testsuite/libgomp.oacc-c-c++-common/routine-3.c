/* At -O0, we do get the expected "undefined reference to `foo'" link-time
   error message (but the check needs to be done differently; compare to
   routine-nohost-1.c), but for -O2 we don't; presumably because the function
   gets inlined.
   { dg-xfail-if "TODO" { *-*-* } { "-O0" } { "" } } */

#include <stdlib.h>

#pragma acc routine nohost
int
foo (int n)
{
  if (n == 0 || n == 1)
    return 1;

  return n * n;
}

int
main()
{
  int a, n = 10;

#pragma acc parallel copy (a, n)
  {
    a = foo (n);
  }

  if (a != n * n)
    abort ();

  return 0;
}
