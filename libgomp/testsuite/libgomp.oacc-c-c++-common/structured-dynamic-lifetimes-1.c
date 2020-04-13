/* Test transitioning of data lifetimes between structured and dynamic.  */

/* { dg-skip-if "" { *-*-* } { "-DACC_MEM_SHARED=1" } } */

#include <openacc.h>
#include <assert.h>
#include <stdlib.h>

#define SIZE 1024

void
f1 (void)
{
  char *block1 = (char *) malloc (SIZE);

#ifdef OPENACC_API
  acc_copyin (block1, SIZE);
  acc_copyin (block1, SIZE);
#else
#pragma acc enter data copyin(block1[0:SIZE])
#pragma acc enter data copyin(block1[0:SIZE])
#endif

#pragma acc data copy(block1[0:SIZE])
  {
#ifdef OPENACC_API
    acc_copyin (block1, SIZE);
#else
#pragma acc enter data copyin(block1[0:SIZE])
#endif
  }

  assert (acc_is_present (block1, SIZE));

#ifdef OPENACC_API
  acc_copyout (block1, SIZE);
  assert (acc_is_present (block1, SIZE));
  acc_copyout (block1, SIZE);
  assert (acc_is_present (block1, SIZE));
  acc_copyout (block1, SIZE);
  assert (!acc_is_present (block1, SIZE));
#else
#pragma acc exit data copyout(block1[0:SIZE])
  assert (acc_is_present (block1, SIZE));
#pragma acc exit data copyout(block1[0:SIZE])
  assert (acc_is_present (block1, SIZE));
#pragma acc exit data copyout(block1[0:SIZE])
  assert (!acc_is_present (block1, SIZE));
#endif

  free (block1);
}

void
f2 (void)
{
  char *block1 = (char *) malloc (SIZE);

#ifdef OPENACC_API
  acc_copyin (block1, SIZE);
#else
#pragma acc enter data copyin(block1[0:SIZE])
#endif

#pragma acc data copy(block1[0:SIZE])
  {
#ifdef OPENACC_API
    acc_copyout (block1, SIZE);
#else
#pragma acc exit data copyout(block1[0:SIZE])
#endif
    /* This should stay present until the end of the structured data
       lifetime.  */
    assert (acc_is_present (block1, SIZE));
  }

  assert (!acc_is_present (block1, SIZE));

  free (block1);
}

void
f3 (void)
{
  char *block1 = (char *) malloc (SIZE);

#ifdef OPENACC_API
  acc_copyin (block1, SIZE);
#else
#pragma acc enter data copyin(block1[0:SIZE])
#endif

#pragma acc data copy(block1[0:SIZE])
  {
#ifdef OPENACC_API
    acc_copyout (block1, SIZE);
    acc_copyin (block1, SIZE);
#else
#pragma acc exit data copyout(block1[0:SIZE])
#pragma acc enter data copyin(block1[0:SIZE])
#endif
    assert (acc_is_present (block1, SIZE));
  }

  assert (acc_is_present (block1, SIZE));
#ifdef OPENACC_API
  acc_copyout (block1, SIZE);
#else
#pragma acc exit data copyout(block1[0:SIZE])
#endif
  assert (!acc_is_present (block1, SIZE));

  free (block1);
}

void
f4 (void)
{
  char *block1 = (char *) malloc (SIZE);
  char *block2 = (char *) malloc (SIZE);
  char *block3 = (char *) malloc (SIZE);

#pragma acc data copy(block1[0:SIZE], block2[0:SIZE], block3[0:SIZE])
  {
  /* The first copyin of block2 is the enclosing data region.  This
     "enter data" should make it live beyond the end of this region.
     This works, though the on-target copies of block1, block2 and block3
     will stay allocated until block2 is unmapped because they are bound
     together in a single target_mem_desc.  */
#ifdef OPENACC_API
    acc_copyin (block2, SIZE);
#else
#pragma acc enter data copyin(block2[0:SIZE])
#endif
  }

  assert (!acc_is_present (block1, SIZE));
  assert (acc_is_present (block2, SIZE));
  assert (!acc_is_present (block3, SIZE));

#ifdef OPENACC_API
  acc_copyout (block2, SIZE);
#else
#pragma acc exit data copyout(block2[0:SIZE])
#endif
  assert (!acc_is_present (block2, SIZE));

  free (block1);
  free (block2);
  free (block3);
}

int
main (int argc, char *argv[])
{
  f1 ();
  f2 ();
  f3 ();
  f4 ();
  return 0;
}
