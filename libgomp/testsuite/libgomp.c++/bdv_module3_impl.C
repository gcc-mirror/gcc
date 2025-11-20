// { dg-skip-if "" { *-*-* } }
// Built with bdv_module3.C

module bdv_module3;

#if _OPENMP
#pragma omp begin declare variant match(construct={teams})
int
test ()
{
  return -1;
}
#pragma omp end declare variant
#endif

void
doit ()
{
  if (test () != 0)
    __builtin_abort ();
  #pragma omp teams
  {
    if (test () != -1)
      __builtin_abort ();
  }
  #pragma omp parallel if(0)
  {
    if (test () != 1)
      __builtin_abort ();
  }
}
