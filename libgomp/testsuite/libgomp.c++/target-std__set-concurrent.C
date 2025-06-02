// { dg-do run }
// { dg-additional-options -DMEM_SHARED { target offload_device_shared_as } }

#include <stdlib.h>
#include <time.h>
#include <set>
#include <algorithm>

#define N 4000
#define MAX 16384

void init (int data[])
{
  std::set<int> _set;
  for (int i = 0; i < N; ++i)
    {
      // Avoid duplicates in data array.
      do
	data[i] = rand () % MAX;
      while (_set.find (data[i]) != _set.end ());
      _set.insert (data[i]);
    }
}

bool validate (int sum, int data[])
{
  int total = 0;
  for (int i = 0; i < N; ++i)
    total += data[i];
  return sum == total;
}

int main (void)
{
  int data[N];
  std::set<int> _set;
  int sum = 0;

  srand (time (NULL));
  init (data);

#ifndef MEM_SHARED
  #pragma omp target data map (to: data[:N]) map (alloc: _set)
#endif
    {
      #pragma omp target
	{
#ifndef MEM_SHARED
	  new (&_set) std::set<int> ();
#endif
	  for (int i = 0; i < N; ++i)
	    _set.insert (data[i]);
	}

      #pragma omp target teams distribute parallel for reduction (+:sum)
	for (int i = 0; i < MAX; ++i)
	  if (_set.find (i) != _set.end ())
	    sum += i;

#ifndef MEM_SHARED
      #pragma omp target
	_set.~set ();
#endif
    }

  bool ok = validate (sum, data);
  return ok ? 0 : 1;
}
