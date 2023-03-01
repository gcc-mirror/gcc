// { dg-do run { target offload_device_nonshared_as } }

#include <string.h>
#include <assert.h>

template<typename T>
void foo (T *w)
{
  memset (w, 0, sizeof (T) * 100);

#pragma omp target enter data map(to: w[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      w[j * 10 + i] = i + j;

#pragma omp target update to(([10][10]) w[3:2][1:8])

#pragma omp target exit data map(from: w[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      if (j >= 3 && j < 5 && i >= 1 && i < 9)
	assert (w[j * 10 + i] == i + j);
      else
	assert (w[j * 10 + i] == 0);
}

int main()
{
  int *arr = new int[100];

  foo<int> (arr);

  delete[] arr;

  return 0;
}
