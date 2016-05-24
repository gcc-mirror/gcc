/* Test if duplicate data mappings with acc_copy_in.  */

/* { dg-do run { target openacc_nvidia_accel_selected } } */

#include <stdio.h>
#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  const int N = 256;
  int i;
  unsigned char *h;

  h = (unsigned char *) malloc (N);

  for (i = 0; i < N; i++)
    {
      h[i] = i;
    }

  (void) acc_copyin (h, N);

  fprintf (stderr, "CheCKpOInT\n");
  (void) acc_copyin (h, N);

  free (h);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "\\\[\[0-9a-fA-FxX\]+,\\\+256\\\] already mapped to \\\[\[0-9a-fA-FxX\]+,\\\+256\\\]" } */
/* { dg-shouldfail "" } */
