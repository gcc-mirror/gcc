/* { dg-do run } */

#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define N 64

typedef struct {
  int *arr;
  int size;
} B;

#pragma omp declare mapper (mapB : B myb) map(to: myb.size, myb.arr) \
					  map(tofrom: myb.arr[0:myb.size])
// While GCC handles more, only default is ...
#pragma omp declare mapper (default : B myb) map(to: myb.size, myb.arr) \
					  map(tofrom: myb.arr[0:myb.size])

struct A {
  int *arr1;
  B *arr2;
  int arr3[N];
};

int
main (int argc, char *argv[])
{
  struct A var;

  memset (&var, 0, sizeof var);
  var.arr1 = (int *) calloc (N, sizeof (int));
  var.arr2 = (B *) malloc (sizeof (B));
  var.arr2->arr = (int *) calloc (N, sizeof (float));
  var.arr2->size = N;

  {
    // ... permitted here:
    #pragma omp declare mapper (struct A x) map(to: x.arr1, x.arr2) \
			  map(tofrom: x.arr1[0:N]) \
			  map(mapper(default), tofrom: x.arr2[0:1])
    #pragma omp target
    {
      for (int i = 0; i < N; i++)
	{
	  var.arr1[i]++;
	  var.arr2->arr[i]++;
	}
    }
  }

  for (int i = 0; i < N; i++)
    {
      assert (var.arr1[i] == 1);
      assert (var.arr2->arr[i] == 1);
      assert (var.arr3[i] == 0);
    }

  free (var.arr1);
  free (var.arr2->arr);
  free (var.arr2);

  return 0;
}
