/* { dg-skip-if "" { *-*-* } { "-DACC_MEM_SHARED=1" } } */

#include <openacc.h>
#include <assert.h>
#include <stdlib.h>

#define SIZE 1024

int
main (int argc, char *argv[])
{
  char *block1 = (char *) malloc (SIZE);
  char *block2 = (char *) malloc (SIZE);

#ifdef OPENACC_API
  acc_copyin (block1, SIZE);
#else
#pragma acc enter data copyin(block1[0:SIZE])
#endif

#pragma acc data copy(block1[0:SIZE], block2[0:SIZE])
  {
#ifdef OPENACC_API
    acc_copyout (block1, SIZE);
    acc_copyin (block2, SIZE);
#else
#pragma acc exit data copyout(block1[0:SIZE])
#pragma acc enter data copyin(block2[0:SIZE])
#endif
    assert (acc_is_present (block1, SIZE));
    assert (acc_is_present (block2, SIZE));
  }

  assert (!acc_is_present (block1, SIZE));
  assert (acc_is_present (block2, SIZE));
#ifdef OPENACC_API
  acc_copyout (block2, SIZE);
#else
#pragma acc exit data copyout(block2[0:SIZE])
#endif
  assert (!acc_is_present (block2, SIZE));

  free (block1);
  free (block2);

  return 0;
}
