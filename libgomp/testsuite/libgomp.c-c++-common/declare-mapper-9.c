/* { dg-do run { target c++ } } */

#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define N 64

struct A {
  int *arr1;
  float *arr2;
  int arr3[N];
};

int
main (int argc, char *argv[])
{
  struct A var;

  memset (&var, 0, sizeof var);
  var.arr1 = (int *) calloc (N, sizeof (int));
  var.arr2 = (float *) calloc (N, sizeof (float));

  {
    #pragma omp declare mapper (struct A x) map(to: x.arr1) \
					    map(tofrom: x.arr1[0:N])
    #pragma omp target
    {
      for (int i = 0; i < N; i++)
	var.arr1[i]++;
    }
  }

  {
    #pragma omp declare mapper (struct A x) map(to: x.arr2) \
					    map(tofrom: x.arr2[0:N])
    #pragma omp target
    {
      for (int i = 0; i < N; i++)
	var.arr2[i]++;
    }
  }

  {
    #pragma omp declare mapper (struct A x) map(tofrom: x.arr3[0:N])
    #pragma omp target
    {
      for (int i = 0; i < N; i++)
	var.arr3[i]++;
    }
  }

  for (int i = 0; i < N; i++)
    {
      assert (var.arr1[i] == 1);
      assert (var.arr2[i] == 1);
      assert (var.arr3[i] == 1);
    }

  free (var.arr1);
  free (var.arr2);
}
