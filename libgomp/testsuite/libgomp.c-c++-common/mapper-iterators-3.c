/* { dg-do run } */

#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define N 64
#define DIM 4

#ifdef DEBUG
#undef DEBUG
#define DEBUG(...) __builtin_printf (__VA_ARGS__)
#else
#define DEBUG(...)
#endif

typedef struct {
  int *arr;
  int size;
} B;

#pragma omp declare mapper (mapB : B myb) map(to: myb.size, myb.arr) \
					  map(tofrom: myb.arr[0:myb.size+1])

struct A {
  int *arr1;
  B *arr2;
  int arr3[N];
};

int
main (int argc, char *argv[])
{
  struct A var[DIM];

  for (int i=0; i < DIM; i++)
    {
      memset (&var[i], 0, sizeof var[i]);
      var[i].arr1 = (int *) calloc (N, sizeof (int));
      var[i].arr2 = (B *) malloc (sizeof (B));
      var[i].arr2->arr = (int *) calloc (N+1, sizeof (float));
      var[i].arr2->size = N+1;
      DEBUG ("host &var[%d]:%p\n", i, &var[i]);
      DEBUG ("host var[%d].arr1:%p\n", i, var[i].arr1);
      DEBUG ("host var[%d].arr2:%p\n", i, var[i].arr2);
      DEBUG ("host var[%d].arr2->arr:%p\n", i, var[i].arr2->arr);
      DEBUG ("host var[%d].arr2->size:%d\n", i, var[i].arr2->size);
    }

  {
    #pragma omp declare mapper (struct A x) map(to: x.arr1, x.arr2) \
			  map(tofrom: x.arr1[0:N]) \
			  map(mapper(mapB), tofrom: x.arr2[0:1])
    #pragma omp target map(iterator(int i=0:DIM), tofrom: var[i])
    {
      for (int i = 0; i < DIM; i++)
	{
	  DEBUG ("&var[%d]:%p\n", i, &var[i]);
	  DEBUG ("var[%d].arr1:%p\n", i, var[i].arr1);
	  DEBUG ("var[%d].arr2:%p\n", i, var[i].arr2);
	  if (var[i].arr2)
	    {
	      DEBUG ("var[%d].arr2->arr:%p\n", i, var[i].arr2->arr);
	      DEBUG ("var[%d].arr2->size:%d\n", i, var[i].arr2->size);
	    }
	  for (int j = 0; j < N; j++)
	    {
	      DEBUG ("&var[%d].arr1[%d]:%p\n", i, j, &var[i].arr1[j]);
	      var[i].arr1[j]++;
	      if (var[i].arr2)
		{
		  DEBUG ("&var[%d].arr2->arr[%d]:%p\n", i, j, &var[i].arr2->arr[j]);
		  var[i].arr2->arr[j]++;
		}
	      else
		DEBUG ("SKIP arr2\n");
	    }
	}
    }
  }

  for (int i = 0; i < DIM; i++)
    for (int j = 0; j < N; j++)
      {
	assert (var[i].arr1[j] == 1);
	assert (var[i].arr2->arr[j] == 1);
	assert (var[i].arr3[j] == 0);
      }

  for (int i = 0; i < DIM; i++)
    {
      free (var[i].arr1);
      free (var[i].arr2->arr);
      free (var[i].arr2);
    }

  return 0;
}
