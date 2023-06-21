/* { dg-do run { target { offload_device } } } */
/* { dg-set-target-env-var OMP_TARGET_OFFLOAD "mandatory" } */

/* Should pass - see target-55a.c for !offload_device */

/* Check OMP_TARGET_OFFLOAD - it shall run on systems with offloading
   devices available and fail otherwise.  Note that this did always
   fail - as the device handling wasn't initialized before doing the
   mandatory checking.  */

int
main ()
{
  int x = 1;
  #pragma omp target map(tofrom: x)
    x = 5;
  if (x != 5)
    __builtin_abort ();
  return 0;
}
