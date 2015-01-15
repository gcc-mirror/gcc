/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <stdlib.h>
#include <openacc.h>
#include <stdint.h>

int
main (int argc, char **argv)
{
  const int N = 256;
  unsigned char *h;
  int i;
  void *d;

  h = (unsigned char *) malloc (N);

  d = acc_malloc (N);

  for (i = 0; i < N; i++)
    {
      acc_map_data ((void *)((uintptr_t) h + (uintptr_t) i),
                    				(void *)((uintptr_t) d + (uintptr_t) i), 1);
    }

  for (i = 0; i < N; i++)
    {
      if (acc_is_present (h + 1, 1) != 1)
	abort ();
    }

  for (i = 0; i < N; i++)
    {
      acc_unmap_data (h + i);
    }

  for (i = 0; i < N; i++)
    {
      if (acc_is_present (h + 1, 1) != 0)
	abort ();
    }

  acc_free (d);

  free (h);

  return 0;
}
