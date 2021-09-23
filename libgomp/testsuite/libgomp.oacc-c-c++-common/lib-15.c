/* Check acc_is_present and acc_copyout.  */

/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

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

  acc_copyout (h, N);

  for (i = 0; i < N; i++)
    {
      if (acc_is_present (h + i, 1) != 0)
	abort ();
    }

  free (h);

  return 0;
}
