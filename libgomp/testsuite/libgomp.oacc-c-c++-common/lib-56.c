/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  const int N = 256;
  unsigned char *h;
  void *d;

  h = (unsigned char *) malloc (N);

  d = acc_malloc (N);

  acc_map_data (h, d, N >> 1);

  if (acc_is_present (h, 1) != 1)
    abort ();

  if (acc_is_present (h + (N >> 1), 1) != 0)
    abort ();

  acc_unmap_data (h);

  acc_free (d);

  free (h);

  return 0;
}
