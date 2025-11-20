// { dg-skip-if "" { *-*-* } }
// Built with bdv_module1.C

import bdv_module1;

int
main ()
{
  if (test () != 0)
    __builtin_abort ();
  #pragma omp parallel if(0)
  {
    if (test () != 1)
      __builtin_abort ();
  }
}
