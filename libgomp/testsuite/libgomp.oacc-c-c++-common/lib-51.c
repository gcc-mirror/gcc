/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

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
      h[i] = (unsigned char *) malloc (N);
      d[i] = acc_malloc (N);

      acc_map_data (h[i], d[i], N);
    }

  for (i = 0; i < N; i++)
    {
      if (acc_is_present (h[i], N) != 1)
	abort ();
    }

  for (i = 0; i < N; i++)
    {
      acc_unmap_data (h[i]);

      if (acc_is_present (h[i], N) != 0)
	abort ();

      acc_free (d[i]);
      free (h[i]);
    }

  return 0;
}
