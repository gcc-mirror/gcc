// { dg-do run { target offload_device_nonshared_as } }

#include <assert.h>
#include <string.h>

#define N 10

struct B {
  int (&aref)[N][N];

  B(int (&aref1)[N][N]) : aref(aref1)
  {
  }
};

template<typename T, int S>
struct C {
  T (&aref)[S][S];

  C(T (&aref1)[S][S]) : aref(aref1)
  {
  }
};

template<typename T>
void foo (T *c)
{
#pragma omp target enter data map(to: c->aref)

#pragma omp target
  {
    for (int i = 0; i < 10; i++)
      for (int j = 0; j < 10; j++)
	c->aref[i][j] = 2 * (i + j);
  }

#pragma omp target update from(c->aref[2:3:2][7:3])

  for (int i = 2; i < 8; i += 2)
    for (int j = 7; j < 10; j++)
      assert (c->aref[i][j] == 2 * (i + j));

#pragma omp target exit data map(delete: c->aref)
}

int main()
{
  int iarr[N][N];
  float farr[N][N];
  B bvar(iarr);
  C<float, N> cvar(farr);

  memset (iarr, 0, N * N * sizeof (int));
  memset (farr, 0, N * N * sizeof (float));

#pragma omp target enter data map(to: bvar.aref)

#pragma omp target
  {
    for (int i = 0; i < 10; i++)
      for (int j = 0; j < 10; j++)
	bvar.aref[i][j] = i + j;
  }

#pragma omp target update from(bvar.aref[4:3][4:3])

  for (int i = 4; i < 7; i++)
    for (int j = 4; j < 7; j++)
      assert (bvar.aref[i][j] == i + j);

#pragma omp target exit data map(delete: bvar.aref)

#pragma omp target enter data map(to: cvar.aref)

#pragma omp target
  {
    for (int i = 0; i < 10; i++)
      for (int j = 0; j < 10; j++)
	cvar.aref[i][j] = i + j;
  }

#pragma omp target update from(cvar.aref[4:3][4:3])

  for (int i = 4; i < 7; i++)
    for (int j = 4; j < 7; j++)
      assert (cvar.aref[i][j] == i + j);

#pragma omp target exit data map(delete: cvar.aref)

  memset (farr, 0, N * N * sizeof (float));

  foo<C<float, N> > (&cvar);

  return 0;
}
