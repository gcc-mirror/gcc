// { dg-do run }
// { dg-additional-options -DMEM_SHARED { target offload_device_shared_as } }

#include <stdlib.h>
#include <time.h>
#include <array>
#include <algorithm>

#define N 50000

void init (int data[])
{
  for (int i = 0; i < N; ++i)
    data[i] = rand ();
}

#pragma omp declare target
bool validate (const std::array<int,N> &arr, int data[])
{
  for (int i = 0; i < N; ++i)
    if (arr[i] != data[i] * data[i])
      return false;
  return true;
}
#pragma omp end declare target

int main (void)
{
  int data[N];
  bool ok;
  std::array<int,N> arr;

  srand (time (NULL));
  init (data);

#ifndef MEM_SHARED
  #pragma omp target data map (to: data[:N]) map (alloc: arr)
#endif
    {
      #pragma omp target
	{
#ifndef MEM_SHARED
	  new (&arr) std::array<int,N> ();
#endif
	  std::copy (data, data + N, arr.begin ());
	}

      #pragma omp target teams distribute parallel for
	for (int i = 0; i < N; ++i)
	  arr[i] *= arr[i];

      #pragma omp target map (from: ok)
	{
	  ok = validate (arr, data);
#ifndef MEM_SHARED
	  arr.~array ();
#endif
	}
    }

  return ok ? 0 : 1;
}
