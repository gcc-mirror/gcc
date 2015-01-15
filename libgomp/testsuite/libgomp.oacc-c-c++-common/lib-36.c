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

  d = acc_present_or_create (h, 0);
  if (!d)
    abort ();

  acc_delete (h, N);

  free (h);

  return 0;
}

/* { dg-shouldfail "libgomp: \[\h+,\+0\] is a bad range" } */
