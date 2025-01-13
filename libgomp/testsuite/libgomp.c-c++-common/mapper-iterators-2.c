/* { dg-do run } */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#define DIM1 4
#define DIM2 16

#ifdef DEBUG
#undef DEBUG
#define DEBUG(...) __builtin_printf (__VA_ARGS__)
#else
#define DEBUG(...)
#endif

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

  /* This should be equivalent to map(iterator(int i=0:DIM1), to: arr[i])  */
  #pragma omp target map(iterator(int i=0:DIM1:2, j=0:2), to: arr[i+j]) map(from: sum)
    {
      sum = 0;
      DEBUG ("&sum: %p\n", &sum);
      for (int i = 0; i < DIM1; i++)
	{
	  DEBUG ("&arr[%d] = %p\n", i, &arr[i]);
	  DEBUG ("arr[%d].len = %d\n", i, arr[i].len);
	  DEBUG ("arr[%d].arr1 = %p\n", i, arr[i].arr1);
	  DEBUG ("arr[%d].arr2 = %p\n", i, arr[i].arr2);
	  for (int j = 0; j < DIM2; j++)
	    {
	      DEBUG ("(i=%d,j=%d): %p\n", i, j, &arr[i].arr1[j]);
	      DEBUG ("(i=%d,j=%d): %d\n", i, j, arr[i].arr1[j]);
	      sum += arr[i].arr1[j];
	      DEBUG ("sum: %ld\n", sum);
	    }
	}
    }

  DEBUG ("&sum: %p\n", &sum);
  DEBUG ("sum:%zd (expected: %zd)\n", sum, expected);

  return sum != expected;
}
