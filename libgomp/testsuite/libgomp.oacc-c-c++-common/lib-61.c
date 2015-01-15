/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <string.h>
#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  const int N = 256;
  int i;
  unsigned char *h[N];
  void *d[N];

  for (i = 0; i < N; i++)
    {
      int j;
      unsigned char *p;

      h[i] = (unsigned char *) malloc (N);

      p = h[i];

      for (j = 0; j < N; j++)
	{
	  p[j] = i;
	}

      d[i] = acc_malloc (N);

      acc_memcpy_to_device (d[i], h[i], N);

      for (j = 0; j < N; j++)
	{
	  if (acc_is_present (h[i] + j, 1) != 0)
	    abort ();
	}
    }

  for (i = 0; i < N; i++)
    {
      int j;
      unsigned char *p;

      memset (h[i], 0, N);

      acc_memcpy_from_device (h[i], d[i], N);

      p = h[i];

      for (j = 0; j < N; j++)
	{
	  if (p[j] != i)
	    abort ();
	}

      for (j = 0; j < N; j++)
	{
	  if (acc_is_present (h[i] + j, 1) != 0)
	    abort ();
	}

      acc_free (d[i]);

      free (h[i]);
    }

  return 0;
}
