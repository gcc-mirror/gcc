#include <cstring>
#include <cassert>
#include <cstdlib>

template<typename T>
struct t_array_wrapper {
  T *data;
  unsigned int length;
};

template<typename T>
void foo()
{
  struct t_array_wrapper<T> aw;

  aw.data = new T[100];
  aw.length = 100;

#pragma omp target enter data map(to: aw.data, aw.length) \
			      map(alloc: aw.data[0:aw.length])

#pragma omp target
  for (int i = 0; i < aw.length; i++)
    aw.data[i] = i;

#pragma omp target update from(aw.data[:aw.length])

#pragma omp target exit data map(delete: aw.data, aw.length, \
				 aw.data[0:aw.length])

  for (int i = 0; i < aw.length; i++)
    assert (aw.data[i] == i);

  delete[] aw.data;
}

struct array_wrapper {
  int *data;
  unsigned int length;
};

int
main ()
{
  struct array_wrapper aw;

  aw.data = new int[100];
  aw.length = 100;

#pragma omp target enter data map(to: aw.data, aw.length) \
			      map(alloc: aw.data[0:aw.length])

#pragma omp target
  for (int i = 0; i < aw.length; i++)
    aw.data[i] = i;

#pragma omp target update from(aw.data[:aw.length])

#pragma omp target exit data map(delete: aw.data, aw.length, \
				 aw.data[0:aw.length])

  for (int i = 0; i < aw.length; i++)
    assert (aw.data[i] == i);

  delete[] aw.data;

  foo<unsigned long> ();

  return 0;
}

