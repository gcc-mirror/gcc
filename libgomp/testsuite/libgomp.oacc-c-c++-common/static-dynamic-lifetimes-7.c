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
/* We can't attach the dynamic data mapping's (block1) target_mem_desc to the
   enclosing static data region here, because that region maps block2 also.  */
#ifdef OPENACC_API
    acc_copyout (block1, SIZE);
#else
#pragma acc exit data copyout(block1[0:SIZE])
#endif
    /* These should stay present until the end of the static data lifetime.  */
    assert (acc_is_present (block1, SIZE));
    assert (acc_is_present (block2, SIZE));
  }

  assert (!acc_is_present (block1, SIZE));
  assert (!acc_is_present (block2, SIZE));

  free (block1);
  free (block2);

  return 0;
}
