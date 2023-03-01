// { dg-do run { target offload_device_nonshared_as } }

#include <string.h>
#include <assert.h>

template<typename T, auto C>
void foo (T *w, int e, int f, int g)
{
  memset (w, 0, sizeof (T) * 100);

#pragma omp target enter data map(to: w[:100])

  for (int j = 0; j < e; j++)
    for (int i = 0; i < C; i++)
      w[j * C + i] = i + j;

#pragma omp target update to(([e][C]) w[3:2][f:g])

#pragma omp target exit data map(from: w[:100])

  for (int j = 0; j < e; j++)
    for (int i = 0; i < C; i++)
      if (j >= 3 && j < 5 && i >= f && i < f + g)
	assert (w[j * C + i] == i + j);
      else
	assert (w[j * C + i] == 0.0f);
}

int main()
{
  float *arr = new float[100];

  foo<float, 10> (arr, 10, 1, 8);

  delete[] arr;

  return 0;
}
