/* { dg-do run } */

#include <stdio.h>
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

#if ACC_MEM_SHARED
  fprintf (stderr, "CheCKpOInT\n");
#endif
  acc_map_data (h, d, N);

#if !ACC_MEM_SHARED
  fprintf (stderr, "CheCKpOInT\n");
#endif
  acc_unmap_data (0);

  acc_free (d);

  free (h);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "\[^\n\r]* is not a mapped block" { target { ! openacc_host_selected } } } */
/* { dg-output "cannot map data on shared-memory system" { target openacc_host_selected } } */
/* { dg-shouldfail "" } */
