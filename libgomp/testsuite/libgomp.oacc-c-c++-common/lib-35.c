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

  d = acc_present_or_create (0, N);
  if (!d)
    abort ();

  acc_delete (h, N);

  free (h);

  return 0;
}

/* { dg-shouldfail "libgomp: \[\(nil\),+256\] is a bad range" } */
