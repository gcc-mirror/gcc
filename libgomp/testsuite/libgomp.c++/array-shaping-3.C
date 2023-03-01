// { dg-do run { target offload_device_nonshared_as } }

#include <string.h>
#include <assert.h>

template<int C, int D>
void foo (double *w)
{
  memset (w, 0, sizeof (double) * 100);

#pragma omp target enter data map(to: w[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      w[j * 10 + i] = i * 3 + j * 2;

#pragma omp target update to(([C][D]) w[3:2][1:8])

#pragma omp target exit data map(from: w[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      if (j >= 3 && j < 5 && i >= 1 && i < 9)
	assert (w[j * 10 + i] == i * 3 + j * 2);
      else
	assert (w[j * 10 + i] == 0.0f);
}

int main()
{
  double *arr = new double[100];

  foo<10, 10> (arr);

  delete[] arr;

  return 0;
}
