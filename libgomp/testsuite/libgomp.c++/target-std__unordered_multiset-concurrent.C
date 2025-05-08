// { dg-do run }
// { dg-additional-options -DMEM_SHARED { target offload_device_shared_as } }

#include <stdlib.h>
#include <time.h>
#include <unordered_set>
#include <algorithm>

// MAX should be less than N to ensure that some duplicates occur.
#define N 4000
#define MAX 1000

void init (int data[])
{
  for (int i = 0; i < N; ++i)
    data[i] = rand () % MAX;
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
  std::unordered_multiset<int> set;
  int sum = 0;

  srand (time (NULL));
  init (data);

  #pragma omp target data map (to: data[:N]) map (alloc: set)
    {
      #pragma omp target
	{
#ifndef MEM_SHARED
	  new (&set) std::unordered_multiset<int> ();
#endif
	  for (int i = 0; i < N; ++i)
	    set.insert (data[i]);
	}

      #pragma omp target teams distribute parallel for reduction (+:sum)
	for (int i = 0; i < MAX; ++i)
	  sum += i * set.count (i);

#ifndef MEM_SHARED
      #pragma omp target
	set.~unordered_multiset ();
#endif
    }

  bool ok = validate (sum, data);
  return ok ? 0 : 1;
}
