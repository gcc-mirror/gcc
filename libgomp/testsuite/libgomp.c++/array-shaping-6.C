// { dg-do run { target offload_device_nonshared_as } }

#include <assert.h>
#include <string.h>

template<typename T>
void foo (T *&aref)
{
#pragma omp target enter data map(to: aref[:100])

#pragma omp target
  {
    for (int i = 0; i < 10; i++)
      for (int j = 0; j < 10; j++)
	aref[i * 10 + j] = i + j;
  }

#pragma omp target update from(([10][10]) aref[2:3:2][7:3])

  for (int i = 2; i < 8; i += 2)
    for (int j = 7; j < 10; j++)
      assert (aref[i * 10 + j] == i + j);

#pragma omp target exit data map(delete: aref[:100])
}

int main()
{
  float *arr = new float[100];
  float *&w = arr;

  memset (arr, 0, 100 * sizeof (float));

#pragma omp target enter data map(to: w[:100])

#pragma omp target
  {
    for (int i = 0; i < 10; i++)
      for (int j = 0; j < 10; j++)
	w[i * 10 + j] = i + j;
  }

#pragma omp target update from(([10][10]) w[4:3][4:3])

  for (int i = 4; i < 7; i++)
    for (int j = 4; j < 7; j++)
      assert (w[i * 10 + j] == i + j);

#pragma omp target exit data map(delete: w[:100])

  foo<float> (arr);

  delete[] arr;
}
