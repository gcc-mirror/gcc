/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

/* Fortran testcase is at libgomp.oacc-fortran/acc_host_device_ptr.f90.  */

#include <stdlib.h>
#include <openacc.h>
#include <stdint.h>

int
main (int argc, char **argv)
{
  const int N = 256;
  int i;
  unsigned char *h;
  void *d;

  h = (unsigned char *) malloc (N);

  d = acc_malloc (N);

  acc_map_data (h, d, N);

  for (i = 0; i < N; i++)
    {
      if (acc_hostptr ((void *)((uintptr_t) d + (uintptr_t) i)) !=
                            (void *)((uintptr_t) h + (uintptr_t) i))
	abort ();
    }

  for (i = 0; i < N; i++)
    {
      if (acc_deviceptr ((void *)((uintptr_t) h + (uintptr_t) i)) !=
                            (void *)((uintptr_t) d + (uintptr_t) i))
	abort ();
    }

  acc_unmap_data (h);

  for (i = 0; i < N; i++)
    {
      if (acc_hostptr ((void *)((uintptr_t) d + (uintptr_t) i)) != 0)
	abort ();
    }

  for (i = 0; i < N; i++)
    {
      if (acc_deviceptr (h + i) != 0)
	abort ();
    }

  acc_free (d);

  free (h);

  return 0;
}
