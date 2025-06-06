// { dg-do run }
// { dg-additional-options "-std=c++23" }
// { dg-additional-options -DMEM_SHARED { target offload_device_shared_as } }

/* { dg-ice {TODO PR120450} { offload_target_amdgcn && { ! offload_device_shared_as } } }
   { dg-excess-errors {'mkoffload' failure etc.} { xfail { offload_target_amdgcn && { ! offload_device_shared_as } } } }
   (For effective-target 'offload_device_shared_as', we've got '-DMEM_SHARED', and therefore don't invoke the constructor with placement new.)  */

#include <stdlib.h>
#include <time.h>
#include <flat_map>

// Make sure that KEY_MAX is less than N to ensure some duplicate keys.
#define N 3000
#define KEY_MAX 1000

void init (int data[], int max)
{
  for (int i = 0; i < N; ++i)
    data[i] = i % max;
}

bool validate (long long sum, int keys[], int data[])
{
  long long total = 0;
  for (int i = 0; i < N; ++i)
    total += (long long) keys[i] * data[i];
  return sum == total;
}

int main (void)
{
  int keys[N], data[N];
  std::flat_multimap<int,int> _map;

  srand (time (NULL));
  init (keys, KEY_MAX);
  init (data, RAND_MAX);

  #pragma omp target enter data map (to: keys[:N], data[:N]) map (alloc: _map)

  #pragma omp target
    {
#ifndef MEM_SHARED
      new (&_map) std::flat_multimap<int,int> ();
#endif
      for (int i = 0; i < N; ++i)
	_map.insert({keys[i], data[i]});
    }

  long long sum = 0;
  #pragma omp target teams distribute parallel for reduction (+:sum)
    for (int i = 0; i < KEY_MAX; ++i)
      {
	auto range = _map.equal_range (i);
	for (auto it = range.first; it != range.second; ++it) {
	  sum += (long long) it->first * it->second;
	}
      }

#ifndef MEM_SHARED
  #pragma omp target
    _map.~flat_multimap ();
#endif

  #pragma omp target exit data map (release: _map)

  bool ok = validate (sum, keys, data);
  return ok ? 0 : 1;
}
