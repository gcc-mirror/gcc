/* Exercise acc_create, acc_is_present and acc_delete.  */

/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  const int N = 256;
  int i;
  unsigned char *h;
  void *d;

  h = (unsigned char *) malloc (N);

  d = acc_create (h, N);
  if (!d)
    abort ();

  for (i = 0; i < N; i++)
    {
      if (acc_is_present (h + i, 1) != 1)
	abort ();
    }

  acc_delete (h, N);

  for (i = 0; i < N; i++)
    {
      if (acc_is_present (h + i, 1) != 0)
	abort ();
    }

  d = acc_create (h, N);
  if (!d)
    abort ();

  for (i = 0; i < N; i++)
    {
      if (acc_is_present (h + i, 1) != 1)
	abort ();
    }

  acc_delete (h, N);

  for (i = 0; i < N; i++)
    {
      if (acc_is_present (h + i, 1) != 0)
	abort ();
    }

  free (h);

  return 0;
}
