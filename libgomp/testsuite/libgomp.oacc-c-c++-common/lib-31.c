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

  d = acc_present_or_create (h, N);
  if (!d)
    abort ();

  if (acc_is_present (h, 1) != 1)
    abort ();

  acc_delete (h, N);

  free (h);

  return 0;
}
