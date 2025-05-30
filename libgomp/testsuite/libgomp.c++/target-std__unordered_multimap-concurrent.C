// { dg-do run }
// { dg-additional-options -DMEM_SHARED { target offload_device_shared_as } }

#include <stdlib.h>
#include <time.h>
#include <unordered_map>

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
  std::unordered_multimap<int,int> _map;

  srand (time (NULL));
  init (keys, KEY_MAX);
  init (data, RAND_MAX);

#ifndef MEM_SHARED
  #pragma omp target enter data map (to: keys[ :N], data[ :N]) map (alloc: _map)
#endif

  #pragma omp target
    {
#ifndef MEM_SHARED
      new (&_map) std::unordered_multimap<int,int> ();
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

#ifdef OMP_USM
  #pragma omp target
    /* Restore the object into pristine state.  In particular, deallocate
       any memory allocated during device execution, which otherwise, back
       on the host, we'd SIGSEGV on, when attempting to deallocate during
       destruction of the object.  */
    __typeof__ (_map){}.swap (_map);
#endif
#ifndef MEM_SHARED
  #pragma omp target
    _map.~unordered_multimap ();
#endif

#ifndef MEM_SHARED
  #pragma omp target exit data map (release: _map)
#endif

  bool ok = validate (sum, keys, data);
  return ok ? 0 : 1;
}
