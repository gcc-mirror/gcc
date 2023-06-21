/* Check OMP_TARGET_OFFLOAD on systems with no available non-host devices,
   which is enforced by using -foffload=disable.  */

/* { dg-do run } */
/* { dg-additional-options "-foffload=disable" } */
/* { dg-set-target-env-var OMP_TARGET_OFFLOAD "mandatory" } */
/* { dg-set-target-env-var OMP_DISPLAY_ENV "true" } */

/* See comment in target-50.c/target-50.c for why default-device-var can be '0'.  */

/* { dg-output ".*OMP_DEFAULT_DEVICE = '-4'.*OMP_TARGET_OFFLOAD = 'MANDATORY'.*" { target { ! offload_device } } } */
/* { dg-output ".*OMP_DEFAULT_DEVICE = '0'.*OMP_TARGET_OFFLOAD = 'MANDATORY'.*" { target offload_device  } } */

int
main ()
{
  return 0;
}
