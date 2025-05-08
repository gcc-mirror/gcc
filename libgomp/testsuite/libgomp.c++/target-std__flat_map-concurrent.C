// { dg-do run }
// { dg-additional-options "-std=c++23" }
// { dg-additional-options -DMEM_SHARED { target offload_device_shared_as } }

/* { dg-ice {TODO PR120450} { offload_target_amdgcn && { ! offload_device_shared_as } } }
   { dg-excess-errors {'mkoffload' failure etc.} { xfail { offload_target_amdgcn && { ! offload_device_shared_as } } } }
   (For effective-target 'offload_device_shared_as', we've got '-DMEM_SHARED', and therefore don't invoke the constructor with placement new.)  */

#include <stdlib.h>
#include <time.h>
#include <set>
#include <flat_map>

#define N 3000

void init (int data[], bool unique)
{
  std::set<int> _set;
  for (int i = 0; i < N; ++i)
    {
      // Avoid duplicates in data array if unique is true.
      do
	data[i] = rand ();
      while (unique && _set.count (data[i]) > 0);
      _set.insert (data[i]);
    }
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
  std::flat_map<int,int> _map;

  srand (time (NULL));
  init (keys, true);
  init (data, false);

  #pragma omp target enter data map (to: keys[:N], data[:N]) map (alloc: _map)

  #pragma omp target
    {
#ifndef MEM_SHARED
      new (&_map) std::flat_map<int,int> ();
#endif
      for (int i = 0; i < N; ++i)
	_map[keys[i]] = data[i];
    }

  long long sum = 0;
  #pragma omp target teams distribute parallel for reduction (+:sum)
    for (int i = 0; i < N; ++i)
      sum += (long long) keys[i] * _map[keys[i]];

#ifndef MEM_SHARED
  #pragma omp target
    _map.~flat_map ();
#endif

  #pragma omp target exit data map (release: _map)

  bool ok = validate (sum, keys, data);
  return ok ? 0 : 1;
}
