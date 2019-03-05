/* { dg-do run } */
/* { dg-require-effective-target tls_runtime } */

#include <omp.h>
#include <stdlib.h>

int t = 128;
#pragma omp threadprivate (t)

int
main ()
{
  #pragma omp parallel
  t = omp_get_thread_num () + 256;
  #pragma omp parallel
  if (t != omp_get_thread_num () + 256)
    abort ();
  omp_pause_resource (omp_pause_soft, omp_get_initial_device ());
  /* This goes beyond what is required by the standard, we actually
     check if the threads other than the initial one have been destroyed.  */
  #pragma omp parallel
  {
    if (omp_get_thread_num () != 0 && t != 128)
      abort ();
    t = omp_get_thread_num () + 384;
  }
  #pragma omp parallel
  if (t != omp_get_thread_num () + 384)
    abort ();
  omp_pause_resource_all (omp_pause_hard);
  #pragma omp parallel
  {
    if (omp_get_thread_num () != 0 && t != 128)
      abort ();
    t = omp_get_thread_num () + 512;
  }
  #pragma omp parallel
  if (t != omp_get_thread_num () + 512)
    abort ();
  return 0;
}
