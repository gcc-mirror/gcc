// { dg-skip-if "" { *-*-* } }
// Built with bdv_module2.C

import bdv_module2;

int
main ()
{
  // Calls to test from doit() should invoke the omp teams variant
  // present in the TU where it is defined.
  doit ();
  // Calls to test from here shouldn't.
  if (test () != 0)
    __builtin_abort ();
  #pragma omp teams
  {
    if (test () != 0)
      __builtin_abort ();
  }
}
