/* { dg-do run } */

#include <stdlib.h>

int i;

int main (void)
{
  int j, v;

#if 0
  i = -1;
  j = -2;
  v = 0;
#pragma acc kernels /* copyout */ present_or_copyout (v) copyin (i, j)
  {
    if (i != -1 || j != -2)
      abort ();
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      abort ();
    v = 1;
  }
  if (v != 1 || i != -1 || j != -2)
    abort ();

  i = -1;
  j = -2;
  v = 0;
#pragma acc kernels /* copyout */ present_or_copyout (v) copyout (i, j)
  {
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      abort ();
    v = 1;
  }
  if (v != 1 || i != 2 || j != 1)
    abort ();

  i = -1;
  j = -2;
  v = 0;
#pragma acc kernels /* copyout */ present_or_copyout (v) copy (i, j)
  {
    if (i != -1 || j != -2)
      abort ();
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      abort ();
    v = 1;
  }
  if (v != 1 || i != 2 || j != 1)
    abort ();

  i = -1;
  j = -2;
  v = 0;
#pragma acc kernels /* copyout */ present_or_copyout (v) create (i, j)
  {
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      abort ();
    v = 1;
  }
  if (v != 1 || i != -1 || j != -2)
    abort ();
#endif

  i = -1;
  j = -2;
  v = 0;
#pragma acc kernels /* copyout */ present_or_copyout (v) present_or_copyin (i, j)
  {
    if (i != -1 || j != -2)
      abort ();
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      abort ();
    v = 1;
  }
  if (v != 1)
    abort ();
#if ACC_MEM_SHARED
  if (i != 2 || j != 1)
    abort ();
#else
  if (i != -1 || j != -2)
    abort ();
#endif

  i = -1;
  j = -2;
  v = 0;
#pragma acc kernels /* copyout */ present_or_copyout (v) present_or_copyout (i, j)
  {
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      abort ();
    v = 1;
  }
  if (v != 1 || i != 2 || j != 1)
    abort ();

  i = -1;
  j = -2;
  v = 0;
#pragma acc kernels /* copyout */ present_or_copyout (v) present_or_copy (i, j)
  {
    if (i != -1 || j != -2)
      abort ();
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      abort ();
    v = 1;
  }
  if (v != 1 || i != 2 || j != 1)
    abort ();

  i = -1;
  j = -2;
  v = 0;
#pragma acc kernels /* copyout */ present_or_copyout (v) present_or_create (i, j)
  {
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      abort ();
    v = 1;
  }
  if (v != 1)
    abort ();
#if ACC_MEM_SHARED
  if (i != 2 || j != 1)
    abort ();
#else
  if (i != -1 || j != -2)
    abort ();
#endif

#if 0
  i = -1;
  j = -2;
  v = 0;
#pragma acc kernels /* copyout */ present_or_copyout (v) present (i, j)
  {
    if (i != -1 || j != -2)
      abort ();
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      abort ();
    v = 1;
  }
  if (v != 1 || i != 2 || j != 1)
    abort ();
#endif

#if 0
  i = -1;
  j = -2;
  v = 0;
#pragma acc kernels /* copyout */ present_or_copyout (v)
  {
    if (i != -1 || j != -2)
      abort ();
    i = 2;
    j = 1;
    if (i != 2 || j != 1)
      abort ();
    v = 1;
  }
  if (v != 1 || i != 2 || j != 1)
    abort ();
#endif

  return 0;
}
