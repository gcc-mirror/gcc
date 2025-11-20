// { dg-skip-if "" { *-*-* } }
// Built with bdv_module3.C

import bdv_module3;

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
  #pragma omp parallel if(0)
  {
    if (test () != 1)
      __builtin_abort ();
  }
}
