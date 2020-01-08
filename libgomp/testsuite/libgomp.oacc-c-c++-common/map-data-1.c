/* Verify that 'acc_map_data' does not copy data to, and 'acc_unmap_data' does
   not copy data from the device.  */

/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <stdlib.h>
#include <string.h>
#include <openacc.h>

int
main ()
{
  const int c0 = 9;
  const int c1 = 40;
  const int c2 = 47;

  const size_t N = 256;

  unsigned char *h = (unsigned char *) malloc (N);

  void *d = acc_malloc (N);

  memset (h, c0, N); // H <- c0
  acc_memcpy_to_device (d, h, N); // D <- H = c0

  memset (h, c1, N); // H <- c1
  acc_map_data (h, d, N);
  for (size_t i = 0; i < N; ++i)
    if (h[i] != c1)
      abort ();

  acc_memcpy_from_device (h, d, N); // H <- D = c0
  for (size_t i = 0; i < N; ++i)
    if (h[i] != c0)
      abort ();

  memset (h, c2, N); // H <- c2
  acc_unmap_data (h);
  for (size_t i = 0; i < N; ++i)
    if (h[i] != c2)
      abort ();

  acc_memcpy_from_device (h, d, N); // H <- D = c0
  for (size_t i = 0; i < N; ++i)
    if (h[i] != c0)
      abort ();

  acc_free (d);

  free (h);

  return 0;
}
