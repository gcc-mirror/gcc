/* Verify that hard pause does not leave a level-zero artificial team without
   a thread pool.  A target-nowait construct outside a parallel region creates
   such an artificial team.  After its task completes, hard pause may release
   runtime resources, but the following parallel region must recreate any
   thread pool it needs rather than dereferencing a stale null pool.

   Force host fallback so this checks the artificial-team lifecycle without
   requiring an offload device.  This is XFAILed until that lifecycle bug is
   fixed.  */
/* { dg-do run } */
/* { dg-xfail-run-if "artificial-team pause" { *-*-* } } */
/* { dg-set-target-env-var OMP_TARGET_OFFLOAD "DISABLED" } */

#include <omp.h>
#include <stdlib.h>

int
main (void)
{
  int target_value = 0;
  int participants = 0;

  omp_set_dynamic (0);

#pragma omp target nowait map(tofrom : target_value)
  target_value = 1;

#pragma omp taskwait
  if (target_value != 1)
    abort ();

  if (omp_in_parallel ())
    abort ();

  if (omp_pause_resource_all (omp_pause_hard) != 0)
    abort ();

#pragma omp parallel num_threads(2) reduction(+ : participants)
  participants += 1;

  if (participants != 2)
    abort ();

  return 0;
}
