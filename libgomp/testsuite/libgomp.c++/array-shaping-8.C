// { dg-do run { target offload_device_nonshared_as } }

#include <assert.h>
#include <string.h>

template<typename T>
struct C {
  T *&aptr;

  C(T *&aptr_1) : aptr(aptr_1)
  {
  }
};

template<typename T>
void foo (T *c)
{
#pragma omp target enter data map(to: c->aptr, c->aptr[:100])

#pragma omp target
  {
    for (int i = 0; i < 10; i++)
      for (int j = 0; j < 10; j++)
	c->aptr[i * 10 + j] = i + j;
  }

#pragma omp target update from(([10][10]) c->aptr[2:3:2][7:3])

  for (int i = 2; i < 8; i += 2)
    for (int j = 7; j < 10; j++)
      assert (c->aptr[i * 10 + j] == i + j);

#pragma omp target exit data map(delete: c->aptr, c->aptr[:100])
}

int main()
{
  float *arr = new float[100];
  C<float> cvar(arr);

  memset (arr, 0, 100 * sizeof (float));

#pragma omp target enter data map(to: cvar.aptr, cvar.aptr[:100])

#pragma omp target
  {
    for (int i = 0; i < 10; i++)
      for (int j = 0; j < 10; j++)
	cvar.aptr[i * 10 + j] = i + j;
  }

#pragma omp target update from(([10][10]) cvar.aptr[4:3][4:3])

  for (int i = 4; i < 7; i++)
    for (int j = 4; j < 7; j++)
      assert (cvar.aptr[i * 10 + j] == i + j);

#pragma omp target exit data map(delete: cvar.aptr, cvar.aptr[:100])

  foo<C<float> > (&cvar);

  delete[] arr;

  return 0;
}
