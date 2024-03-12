/* Check OMP_TARGET_OFFLOAD on systems with no available non-host devices,
   which is enforced by using -foffload=disable.  */

/* { dg-do run } */
/* { dg-additional-options "-foffload=disable" } */
/* { dg-set-target-env-var OMP_TARGET_OFFLOAD "mandatory" } */

/* { dg-shouldfail "OMP_TARGET_OFFLOAD=mandatory and no available device" } */

/* See comment in target-50.c/target-50.c for why the output differs.  */

/* { dg-output ".*libgomp: OMP_TARGET_OFFLOAD is set to MANDATORY, but .*" } */

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
