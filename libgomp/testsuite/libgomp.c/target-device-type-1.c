/* { dg-do run { target offload_device } } */
/* { dg-additional-options "-fdump-tree-gimple" } */

int
f (int *x)
{
  #pragma omp target map(tofrom:x[0:1]) device_type(nohost)
    (*x)++;
  return *x;
}

int
main ()
{
  int i = 3;
  if (f(&i) != 4)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump "= __builtin_omp_is_initial_device \\(\\);" "gimple" } } */
/* { dg-final { scan-tree-dump "__builtin_GOMP_error \\(&\"Executing device-type ..nohost.. target region on the host\"\\\[0\\\]," "gimple" } } */
