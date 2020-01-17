/* Test nested dynamic/static data mappings (multiple blocks on data
   regions).  */

/* { dg-skip-if "" { *-*-* } { "-DACC_MEM_SHARED=1" } } */

#include <openacc.h>
#include <assert.h>
#include <stdlib.h>

#define SIZE 1024

void
f1 (void)
{
  char *block1 = (char *) malloc (SIZE);
  char *block2 = (char *) malloc (SIZE);

#pragma acc data copy(block1[0:SIZE], block2[0:SIZE])
  {
#ifdef OPENACC_API
    acc_copyin (block1, SIZE);
    acc_copyout (block1, SIZE);
#else
#pragma acc enter data copyin(block1[0:SIZE])
#pragma acc exit data copyout(block1[0:SIZE])
#endif
  }

  assert (!acc_is_present (block1, SIZE));
  assert (!acc_is_present (block2, SIZE));

  free (block1);
  free (block2);
}

void
f2 (void)
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
  }

#ifdef OPENACC_API
  acc_copyout (block1, SIZE);
#else
#pragma acc exit data copyout(block1[0:SIZE])
#endif

  assert (!acc_is_present (block1, SIZE));
  assert (!acc_is_present (block2, SIZE));

  free (block1);
  free (block2);
}

void
f3 (void)
{
  char *block1 = (char *) malloc (SIZE);
  char *block2 = (char *) malloc (SIZE);

#pragma acc data copy(block1[0:SIZE], block2[0:SIZE])
  {
#ifdef OPENACC_API
    acc_copyin (block1, SIZE);
    acc_copyin (block2, SIZE);
    acc_copyout (block2, SIZE);
    acc_copyout (block1, SIZE);
#else
#pragma acc enter data copyin(block1[0:SIZE])
#pragma acc enter data copyin(block2[0:SIZE])
#pragma acc exit data copyout(block2[0:SIZE])
#pragma acc exit data copyout(block1[0:SIZE])
#endif
  }

  assert (!acc_is_present (block1, SIZE));
  assert (!acc_is_present (block2, SIZE));

  free (block1);
  free (block2);
}

void
f4 (void)
{
  char *block1 = (char *) malloc (SIZE);
  char *block2 = (char *) malloc (SIZE);

#pragma acc data copy(block1[0:SIZE], block2[0:SIZE])
  {
#ifdef OPENACC_API
    acc_copyin (block1, SIZE);
#else
#pragma acc enter data copyin(block1[0:SIZE])
#endif

#pragma acc data copy(block1[0:SIZE], block2[0:SIZE])
    {
#ifdef OPENACC_API
      acc_copyin (block2, SIZE);
      acc_copyout (block2, SIZE);
#else
#pragma acc enter data copyin(block2[0:SIZE])
#pragma acc exit data copyout(block2[0:SIZE])
#endif
    }
#ifdef OPENACC_API
  acc_copyout (block1, SIZE);
#else
#pragma acc exit data copyout(block1[0:SIZE])
#endif
  }

  assert (!acc_is_present (block1, SIZE));
  assert (!acc_is_present (block2, SIZE));

  free (block1);
  free (block2);
}

void
f5 (void)
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
    acc_copyin (block2, SIZE);
#else
#pragma acc enter data copyin(block2[0:SIZE])
#endif

#pragma acc data copy(block1[0:SIZE], block2[0:SIZE])
    {
    }
#ifdef OPENACC_API
    acc_copyout (block2, SIZE);
#else
#pragma acc exit data copyout(block2[0:SIZE])
#endif
  }

#ifdef OPENACC_API
  acc_copyout (block1, SIZE);
#else
#pragma acc exit data copyout(block1[0:SIZE])
#endif

  assert (!acc_is_present (block1, SIZE));
  assert (!acc_is_present (block2, SIZE));

  free (block1);
  free (block2);
}

int
main (int argc, char *argv[])
{
  f1 ();
  f2 ();
  f3 ();
  f4 ();
  f5 ();
  return 0;
}
