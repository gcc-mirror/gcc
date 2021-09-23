/* Exercise acc_map_data with data size of zero.  */

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

  fprintf (stderr, "CheCKpOInT\n");
  acc_map_data (h, d, 0);

  acc_unmap_data (h);

  acc_free (d);

  free (h);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "\\\[\[0-9a-fA-FxX\]+,\\\+0\\\]->\\\[\[0-9a-fA-FxX\]+,\\\+0\\\] is a bad map" { target { ! openacc_host_selected } } } */
/* { dg-output "cannot map data on shared-memory system" { target openacc_host_selected } } */
/* { dg-shouldfail "" } */
