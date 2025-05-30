// { dg-do run }
// { dg-additional-options -DMEM_SHARED { target offload_device_shared_as } }

#include <stdlib.h>
#include <time.h>
#include <vector>

#define N 50000

void init (int data[])
{
  for (int i = 0; i < N; ++i)
    data[i] = rand ();
}

#pragma omp declare target
bool validate (const std::vector<int> &vec, int data[])
{
  for (int i = 0; i < N; ++i)
    if (vec[i] != data[i] * data[i])
      return false;
  return true;
}
#pragma omp end declare target

int main (void)
{
  int data[N];
  bool ok;

  srand (time (NULL));
  init (data);

#ifdef MEM_SHARED
  std::vector<int> vec (data, data + N);
#else
  std::vector<int> vec;
#endif

#ifndef MEM_SHARED
  #pragma omp target data map (to: data[:N]) map (alloc: vec)
#endif
    {
#ifndef MEM_SHARED
      #pragma omp target
	new (&vec) std::vector<int> (data, data + N);
#endif

      #pragma omp target teams distribute parallel for
	for (int i = 0; i < N; ++i)
	  vec[i] *= vec[i];

      #pragma omp target map (from: ok)
	{
	  ok = validate (vec, data);
#ifndef MEM_SHARED
	  vec.~vector ();
#endif
	}
    }

  return ok ? 0 : 1;
}
