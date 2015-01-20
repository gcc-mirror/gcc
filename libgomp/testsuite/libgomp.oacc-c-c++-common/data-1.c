/* { dg-do run } */

#include <stdlib.h>
#include <openacc.h>

int i;

int
is_mapped (void *p, size_t n)
{
#if ACC_MEM_SHARED
  return 1;
#else
  return acc_is_present (p, n);
#endif
}

int main(void)
{
  int j;

  i = -1;
  j = -2;
#pragma acc data copyin (i, j)
  {
    if (!is_mapped (&i, sizeof (i)) || !is_mapped (&j, sizeof (j)))
      abort ();
    if (i != -1 || j != -2)
      abort ();
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      abort ();
  }
  if (i != 2 || j != 1)
    abort ();

  i = -1;
  j = -2;
#pragma acc data copyout (i, j)
  {
    if (!is_mapped (&i, sizeof (i)) || !is_mapped (&j, sizeof (j)))
      abort ();
    if (i != -1 || j != -2)
      abort ();
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      abort ();

#pragma acc parallel present (i, j)
    {
      i = 4;
      j = 2;
    }
  }
  if (i != 4 || j != 2)
    abort ();

  i = -1;
  j = -2;
#pragma acc data create (i, j)
  {
    if (!is_mapped (&i, sizeof (i)) || !is_mapped (&j, sizeof (j)))
      abort ();
    if (i != -1 || j != -2)
      abort ();
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      abort ();
  }
  if (i != 2 || j != 1)
    abort ();

  i = -1;
  j = -2;
#pragma acc data present_or_copyin (i, j)
  {
    if (!is_mapped (&i, sizeof (i)) || !is_mapped (&j, sizeof (j)))
      abort ();
    if (i != -1 || j != -2)
      abort ();
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      abort ();
  }
  if (i != 2 || j != 1)
    abort ();

  i = -1;
  j = -2;
#pragma acc data present_or_copyout (i, j)
  {
    if (!is_mapped (&i, sizeof (i)) || !is_mapped (&j, sizeof (j)))
      abort ();
    if (i != -1 || j != -2)
      abort ();
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      abort ();

#pragma acc parallel present (i, j)
    {
      i = 4;
      j = 2;
    }
  }
  if (i != 4 || j != 2)
    abort ();

  i = -1;
  j = -2;
#pragma acc data present_or_copy (i, j)
  {
    if (!is_mapped (&i, sizeof (i)) || !is_mapped (&j, sizeof (j)))
      abort ();
    if (i != -1 || j != -2)
      abort ();
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      abort ();
  }
#if ACC_MEM_SHARED
  if (i != 2 || j != 1)
    abort ();
#else
  if (i != -1 || j != -2)
    abort ();
#endif

  i = -1;
  j = -2;
#pragma acc data present_or_create (i, j)
  {
    if (!is_mapped (&i, sizeof (i)) || !is_mapped (&j, sizeof (j)))
      abort ();
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      abort ();
  }

  if (i != 2 || j != 1)
    abort ();

  i = -1;
  j = -2;
#pragma acc data copyin (i, j)
  {
#pragma acc data present (i, j)
    {
      if (!is_mapped (&i, sizeof (i)) || !is_mapped (&j, sizeof (j)))
        abort ();
      if (i != -1 || j != -2)
        abort ();
      i = 2;
      j = 1;
      if (i != 2 || j != 1)
        abort ();
    }
  }
  if (i != 2 || j != 1)
    abort ();

  i = -1;
  j = -2;
#pragma acc data
  {
#if !ACC_MEM_SHARED
    if (is_mapped (&i, sizeof (i)) || is_mapped (&j, sizeof (j)))
      abort ();
#endif
    if (i != -1 || j != -2)
      abort ();
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      abort ();
  }
  if (i != 2 || j != 1)
    abort ();

  return 0;
}
