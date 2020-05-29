/* Verify that 'acc_unmap_data' unmaps even in presence of structured and
   dynamic reference counts, but the device memory remains allocated.  */

/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <openacc.h>

int
main ()
{
  const int N = 180;
  const int N_i = 537;
  const int C = 37;

  unsigned char *h = (unsigned char *) malloc (N);
  assert (h);
  unsigned char *d = (unsigned char *) acc_malloc (N);
  assert (d);

  for (int i = 0; i < N_i; ++i)
    {
      acc_map_data (h, d, N);
      assert (acc_is_present (h, N));
#pragma acc parallel present(h[0:N])
      {
	if (i == 0)
	  memset (h, C, N);
      }

      unsigned char *d_ = (unsigned char *) acc_create (h + 3, N - 77);
      assert (d_ == d + 3);

#pragma acc data create(h[6:N - 44])
      {
	d_ = (unsigned char *) acc_create (h, N);
	assert (d_ == d);

#pragma acc enter data create(h[0:N])

	assert (acc_is_present (h, N));
	acc_unmap_data (h);
	assert (!acc_is_present (h, N));
      }

      /* We can however still access the device memory.  */
#pragma acc parallel loop deviceptr(d)
      for (int j = 0; j < N; ++j)
	d[j] += i * j;
    }

  acc_memcpy_from_device(h, d, N);
  for (int j = 0; j < N; ++j)
    assert (h[j] == ((C + N_i * (N_i - 1) / 2 * j) % 256));

  acc_free (d);

  return 0;
}
