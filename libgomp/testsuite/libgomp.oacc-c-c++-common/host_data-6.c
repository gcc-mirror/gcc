/* Call 'acc_memcpy_from_device' inside '#pragma acc host_data'.  */

/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <openacc.h>

int
main ()
{
  const int SIZE = 318;
  const int c0 = 22;
  const int c1 = 112;

  char *h = (char *) malloc (SIZE);

  memset (h, c0, SIZE);

#pragma acc data create (h[0:SIZE - 44])
  {
#pragma acc update device (h[0:SIZE - 44])

    memset (h, c1, 67);

    void *d = h;
#pragma acc host_data use_device (d)
    {
      acc_memcpy_from_device (h, d, 12);
    }
  }

  for (int i = 0; i < SIZE; ++i)
    {
      if (i < 12)
	assert (h[i] == c0);
      else if (i < 67)
	assert (h[i] == c1);
      else
	assert (h[i] == c0);
    }

  free (h);

  return 0;
}
