/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "-DACC_MEM_SHARED=1" } } */

/* Test if acc_unmap_data has implicit finalize semantics.  */

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

  acc_map_data (h, d, N);

  acc_copyin (h, N);
  acc_copyin (h, N);
  acc_copyin (h, N);

  acc_unmap_data (h);

  if (acc_is_present (h, N))
    abort ();

  acc_free (d);

  free (h);

  return 0;
}
