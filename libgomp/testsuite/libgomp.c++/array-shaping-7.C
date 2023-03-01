// { dg-do run { target offload_device_nonshared_as } }

#include <assert.h>
#include <string.h>

template<typename T>
void foo (T (&aref)[10][10])
{
#pragma omp target enter data map(to: aref)

#pragma omp target
  {
    for (int i = 0; i < 10; i++)
      for (int j = 0; j < 10; j++)
	aref[i][j] = i + j;
  }

#pragma omp target update from(aref[2:3:2][7:3])

  for (int i = 2; i < 8; i += 2)
    for (int j = 7; j < 10; j++)
      assert (aref[i][j] == i + j);

#pragma omp target exit data map(delete: aref)
}

int main()
{
  float arr2d[10][10];
  float (&w)[10][10] = arr2d;

  memset (&arr2d, 0, 100 * sizeof (float));

#pragma omp target enter data map(to: w)

#pragma omp target
  {
    for (int i = 0; i < 10; i++)
      for (int j = 0; j < 10; j++)
	w[i][j] = i + j;
  }

#pragma omp target update from(w[4:3][4:3])

  for (int i = 4; i < 7; i++)
    for (int j = 4; j < 7; j++)
      assert (w[i][j] == i + j);

#pragma omp target exit data map(delete: w)

  foo<float> (arr2d);

  return 0;
}
