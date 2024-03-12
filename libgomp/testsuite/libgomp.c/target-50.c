/* Check OMP_TARGET_OFFLOAD on systems with no available non-host devices;
   here with using -foffload=disable.
   As default-device-var is set to 0 (= host in this case), it should not fail.  */

/* Note that -foffload=disable will still find devices on the system and only
   when trying to use them, it will fail as no binary data has been produced.
   The "target offload_device" case is checked for in 'target-50a.c'.  */

/* { dg-do run { target { ! offload_device } } } */

/* { dg-additional-options "-foffload=disable" } */
/* { dg-set-target-env-var OMP_TARGET_OFFLOAD "mandatory" } */
/* { dg-set-target-env-var OMP_DEFAULT_DEVICE "0" } */
/* { dg-set-target-env-var OMP_DISPLAY_ENV "true" } */

/* { dg-output ".*OMP_DEFAULT_DEVICE = '0'.*OMP_TARGET_OFFLOAD = 'MANDATORY'.*" } */

int
main ()
{
  int x;
  #pragma omp target map(tofrom:x)
    x = 5;
  if (x != 5)
    __builtin_abort ();
  return 0;
}
