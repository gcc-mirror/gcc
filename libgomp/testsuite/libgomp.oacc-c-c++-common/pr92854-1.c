/* Verify that 'acc_unmap_data' unmaps even in presence of dynamic reference
   counts.  */

/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <assert.h>
#include <stdlib.h>
#include <openacc.h>

int
main ()
{
  const int N = 180;

  char *h = (char *) malloc (N);
  char *d = (char *) acc_malloc (N);
  if (!d)
    abort ();
  acc_map_data (h, d, N);

  char *d_ = (char *) acc_create (h + 3, N - 77);
  assert (d_ == d + 3);

  d_ = (char *) acc_create (h, N);
  assert (d_ == d);

  acc_unmap_data (h);
  assert (!acc_is_present (h, N));

  return 0;
}
