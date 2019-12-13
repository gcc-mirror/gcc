/* Check acc_is_present and acc_delete.  */

/* { dg-do run { target openacc_nvidia_accel_selected } } */

#include <stdlib.h>
#include <openacc.h>

#include <stdio.h>

int
main (int argc, char **argv)
{
  const int N = 256;
  int i;
  unsigned char *h;
  void *d;

  h = (unsigned char *) malloc (N);

  for (i = 0; i < N; i++)
    {
      h[i] = i;
    }

  d = acc_copyin (h, N);

  if (acc_is_present (h, 1) != 1)
    abort ();

  if (acc_is_present (h, N + 1) != 0)
    abort ();

  if (acc_is_present (h + 1, N) != 0)
    abort ();

  if (acc_is_present (h - 1, N) != 0)
    abort ();

  if (acc_is_present (h - 1, N - 1) != 0)
    abort ();

  if (acc_is_present (h + N, 0) != 0)
    abort ();

  if (acc_is_present (h + N, N) != 0)
    abort ();

  if (acc_is_present (0, N) != 0)
    abort ();
   
  if (acc_is_present (h, 0) != 0)
    abort ();

  acc_delete (h, N);

  if (acc_is_present (h, 1) != 0)
    abort ();

  free (h);

  return 0;
}
