/* Exercise acc_map_data with a NULL data mapping.  */

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
  acc_map_data (0, d, N);

  acc_unmap_data (h);

  acc_free (d);

  free (h);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "\\\[\[^\n\r]*,\\\+256\]->\[\[0-9a-fA-FxX\]+,\\\+256\\\] is a bad map" { target { ! openacc_host_selected } } } */
/* { dg-output "cannot map data on shared-memory system" { target openacc_host_selected } } */
/* { dg-shouldfail "" } */
