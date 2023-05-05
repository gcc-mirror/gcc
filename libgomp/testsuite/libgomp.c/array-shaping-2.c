// { dg-do run { target offload_device_nonshared_as } }

#include <assert.h>
#include <stdlib.h>

typedef struct {
  int *aptr;
} C;

int main()
{
  C cvar;

  cvar.aptr = calloc (100, sizeof (float));

#pragma omp target enter data map(to: cvar.aptr, cvar.aptr[:100])

#pragma omp target
  {
    for (int i = 0; i < 10; i++)
      for (int j = 0; j < 10; j++)
	cvar.aptr[i * 10 + j] = i + j;
  }

#pragma omp target update from(([10][10]) cvar.aptr[4:3][4:3])

  for (int i = 0; i < 10; i++)
    for (int j = 0; j < 10; j++)
      if (i >= 4 && i < 7 && j >= 4 && j < 7)
	assert (cvar.aptr[i * 10 + j] == i + j);
      else
	assert (cvar.aptr[i * 10 + j] == 0);

#pragma omp target exit data map(delete: cvar.aptr, cvar.aptr[:100])

  free (cvar.aptr);

  return 0;
}
