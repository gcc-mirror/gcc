/* { dg-do run } */

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

  acc_unmap_data (d);

  acc_free (d);

  free (h);

  return 0;
}

/* { dg-shouldfail "libgomp: \h+ is not a mapped block" } */
