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

  acc_unmap_data (0);

  acc_free (d);

  free (h);

  return 0;
}

/* { dg-output "\[^\n\r]* is not a mapped block" { target openacc_nvidia_accel_selected } } */
/* { dg-output "cannot map data on shared-memory system" { target openacc_host_selected } } */
/* { dg-shouldfail "" } */
