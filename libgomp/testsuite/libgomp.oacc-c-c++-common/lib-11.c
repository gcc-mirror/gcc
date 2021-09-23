/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <stdio.h>
#include <stdlib.h>
#include <openacc.h>
#include <stdint.h>

int
main (int argc, char **argv)
{
  const int N = 512;
  void *d;

  d = acc_malloc (N);
  if (d == NULL)
    abort ();

  fprintf (stderr, "CheCKpOInT\n");
  acc_free ((void *)((uintptr_t) d + (uintptr_t) (N >> 1)));

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r)+" } */
/* { dg-output "libgomp: invalid device address(\n|\r\n|\r)+" { target openacc_nvidia_accel_selected } } */
/* { dg-output "libgomp: GCN fatal error: Could not free device memory(\n|\r\n|\r)+" { target openacc_radeon_accel_selected } }
   { dg-output "Runtime message: HSA_STATUS_ERROR_INVALID_ALLOCATION: The requested allocation is not valid\.(\n|\r\n|\r)+" { target openacc_radeon_accel_selected } } */
/* { dg-output "libgomp: error in freeing device memory in acc_free(\n|\r\n|\r)+$" } */
/* { dg-shouldfail "" } */
