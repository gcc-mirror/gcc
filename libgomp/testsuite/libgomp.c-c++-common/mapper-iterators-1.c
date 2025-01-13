/* { dg-do run } */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#define DIM1 4
#define DIM2 16

struct S {
  int *arr1;
  float *arr2;
  size_t len;
};

size_t
mkarray (struct S arr[])
{
  size_t sum = 0;

  for (int i = 0; i < DIM1; i++)
    {
      memset (&arr[i], 0, sizeof (struct S));
      arr[i].len = DIM2;
      arr[i].arr1 = (int *) calloc (arr[i].len, sizeof (int));
      for (int j = 0; j < DIM2; j++)
	{
	  size_t value = (i + 1) * (j + 1);
	  sum += value;
	  arr[i].arr1[j] = value;
	}
    }

  return sum;
}

int main ()
{
  struct S arr[DIM1];
  size_t sum = 0xdeadbeef;
  size_t expected = mkarray (arr);

  #pragma omp declare mapper (struct S x) \
	map(to: x.arr1[0:DIM2]) \
	map(to: x.arr2[0:DIM2]) \
	map(to: x.len)

  #pragma omp target map(iterator(int i=0:DIM1), to: arr[i]) map(from: sum)
    {
      sum = 0;
#ifdef DEBUG
      __builtin_printf ("&sum: %p\n", &sum);
#endif
      for (int i = 0; i < DIM1; i++)
	{
#ifdef DEBUG
	  __builtin_printf ("&arr[%d] = %p\n", i, &arr[i]);
	  __builtin_printf ("arr[%d].len = %d\n", i, arr[i].len);
	  __builtin_printf ("arr[%d].arr1 = %p\n", i, arr[i].arr1);
	  __builtin_printf ("arr[%d].arr2 = %p\n", i, arr[i].arr2);
#endif
	  for (int j = 0; j < DIM2; j++)
	    {
#ifdef DEBUG
	      __builtin_printf ("(i=%d,j=%d): %p\n", i, j, &arr[i].arr1[j]);
	      __builtin_printf ("(i=%d,j=%d): %d\n", i, j, arr[i].arr1[j]);
#endif
	      sum += arr[i].arr1[j];
#ifdef DEBUG
	      __builtin_printf ("sum: %ld\n", sum);
#endif
	    }
	}
    }

#ifdef DEBUG
  __builtin_printf ("&sum: %p\n", &sum);
  __builtin_printf ("sum:%zd (expected: %zd)\n", sum, expected);
#endif

  return sum != expected;
}
