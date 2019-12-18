/* Verify that 'acc_delete' etc. with zero size is a no-op.  */

#include <assert.h>
#include <stdlib.h>
#include <openacc.h>


#define UNHANDLED_GOMP_MAP_ZERO_LEN_ARRAY_SECTION


static void
verify_mapped_unchanged (unsigned char *a, size_t N)
{
  assert (acc_is_present (a, N));

  for (size_t i = 0; i < N; ++i)
    assert (a[i] == (unsigned char) i);
}

int
main (int argc, char **argv)
{
  const size_t N = 256;

  unsigned char *a = (unsigned char *) malloc (N);
  assert (a);

  for (size_t i = 0; i < N; ++i)
    a[i] = 51;

  void *a_d = acc_copyin (a, N);
  assert (a_d);

  for (size_t i = 0; i < N; ++i)
    a[i] = i;

  int async = 0;

  const size_t size = 0;

#ifndef UNHANDLED_GOMP_MAP_ZERO_LEN_ARRAY_SECTION
#pragma acc exit data copyout (a[0:size])
  verify_mapped_unchanged (a, N);
#endif
  acc_copyout (a, size);
  verify_mapped_unchanged (a, N);
#ifndef UNHANDLED_GOMP_MAP_ZERO_LEN_ARRAY_SECTION
#pragma acc exit data copyout (a[0:size]) async (async++)
  verify_mapped_unchanged (a, N);
#endif
  acc_copyout_async (a, size, async++);
  verify_mapped_unchanged (a, N);
#ifndef UNHANDLED_GOMP_MAP_ZERO_LEN_ARRAY_SECTION
#pragma acc exit data copyout (a[0:size]) finalize
  verify_mapped_unchanged (a, N);
#endif
  acc_copyout_finalize (a, size);
  verify_mapped_unchanged (a, N);
#ifndef UNHANDLED_GOMP_MAP_ZERO_LEN_ARRAY_SECTION
#pragma acc exit data copyout (a[0:size]) finalize async (async++)
  verify_mapped_unchanged (a, N);
#endif
  acc_copyout_finalize_async (a, size, async++);
  verify_mapped_unchanged (a, N);

#ifndef UNHANDLED_GOMP_MAP_ZERO_LEN_ARRAY_SECTION
#pragma acc exit data delete (a[0:size])
  verify_mapped_unchanged (a, N);
#endif
  acc_delete (a, size);
  verify_mapped_unchanged (a, N);
#ifndef UNHANDLED_GOMP_MAP_ZERO_LEN_ARRAY_SECTION
#pragma acc exit data delete (a[0:size]) async (async++)
  verify_mapped_unchanged (a, N);
#endif
  acc_delete_async (a, size, async++);
  verify_mapped_unchanged (a, N);
#ifndef UNHANDLED_GOMP_MAP_ZERO_LEN_ARRAY_SECTION
#pragma acc exit data delete (a[0:size]) finalize
  verify_mapped_unchanged (a, N);
#endif
  acc_delete_finalize (a, size);
  verify_mapped_unchanged (a, N);
#ifndef UNHANDLED_GOMP_MAP_ZERO_LEN_ARRAY_SECTION
#pragma acc exit data delete (a[0:size]) finalize async (async++)
  verify_mapped_unchanged (a, N);
#endif
  acc_delete_finalize_async (a, size, async++);
  verify_mapped_unchanged (a, N);

  acc_wait_all ();

  acc_delete (a, N);
#if !ACC_MEM_SHARED
  assert (!acc_is_present (a, N));
#endif
  free (a);

  return 0;
}
