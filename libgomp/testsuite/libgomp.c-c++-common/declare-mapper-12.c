/* { dg-do run } */

#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define N 64

typedef struct {
  int *arr;
  int size;
} B;

#pragma omp declare mapper (samename : B myb) map(to: myb.size, myb.arr) \
					      map(tofrom: myb.arr[0:myb.size])

typedef struct {
  int *arr;
  int size;
} C;


struct A {
  int *arr1;
  B *arr2;
  C *arr3;
};

int
main (int argc, char *argv[])
{
  struct A var;

  memset (&var, 0, sizeof var);
  var.arr1 = (int *) calloc (N, sizeof (int));
  var.arr2 = (B *) malloc (sizeof (B));
  var.arr2->arr = (int *) calloc (N, sizeof (int));
  var.arr2->size = N;
  var.arr3 = (C *) malloc (sizeof (C));
  var.arr3->arr = (int *) calloc (N, sizeof (int));
  var.arr3->size = N;

  {
    #pragma omp declare mapper (struct A x) map(to: x.arr1, x.arr2) \
			map(tofrom: x.arr1[0:N]) \
			map(mapper(samename), tofrom: x.arr2[0:1])
    #pragma omp target
    {
      for (int i = 0; i < N; i++)
	{
	  var.arr1[i]++;
	  var.arr2->arr[i]++;
	}
    }
  }

  {
    #pragma omp declare mapper (samename : C myc) map(to: myc.size, myc.arr) \
			map(tofrom: myc.arr[0:myc.size])
    #pragma omp declare mapper (struct A x) map(to: x.arr1, x.arr3) \
			map(tofrom: x.arr1[0:N]) \
			map(mapper(samename), tofrom: *x.arr3)
    #pragma omp target
    {
      for (int i = 0; i < N; i++)
	{
	  var.arr1[i]++;
	  var.arr3->arr[i]++;
	}
    }
  }

  for (int i = 0; i < N; i++)
    {
      assert (var.arr1[i] == 2);
      assert (var.arr2->arr[i] == 1);
      assert (var.arr3->arr[i] == 1);
    }

  free (var.arr1);
  free (var.arr2->arr);
  free (var.arr2);
  free (var.arr3->arr);
  free (var.arr3);

  return 0;
}
