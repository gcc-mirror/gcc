/* { dg-do run } */

#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  const int N = 256;
  unsigned char *h;
  void *d1, *d2;

  h = (unsigned char *) malloc (N);

  d1 = acc_present_or_create (h, N);
  if (!d1)
    abort ();

  d2 = acc_present_or_create (h, N - 2);
  if (!d2)
    abort ();

  if (d1 != d2)
    abort ();

  acc_delete (h, N);

  free (h);

  return 0;
}
