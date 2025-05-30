// { dg-do run }
// { dg-additional-options -DMEM_SHARED { target offload_device_shared_as } }

#include <stdlib.h>
#include <time.h>
#include <deque>
#include <algorithm>

#define N 50000

void init (int data[])
{
  for (int i = 0; i < N; ++i)
    data[i] = rand ();
}

#pragma omp declare target
bool validate (const std::deque<int> &_deque, int data[])
{
  for (int i = 0; i < N; ++i)
    if (_deque[i] != data[i] * data[i])
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
  std::deque<int> _deque (std::begin (data), std::end (data));
#else
  std::deque<int> _deque;
#endif

#ifndef MEM_SHARED
  #pragma omp target data map (to: data[:N]) map (alloc: _deque)
#endif
    {
#ifndef MEM_SHARED
      #pragma omp target
	new (&_deque) std::deque<int> (std::begin (data), std::end (data));
#endif

      #pragma omp target teams distribute parallel for
	for (int i = 0; i < N; ++i)
	  _deque[i] *= _deque[i];

      #pragma omp target map (from: ok)
	{
	  ok = validate (_deque, data);
#ifndef MEM_SHARED
	  _deque.~deque ();
#endif
	}
    }

  return ok ? 0 : 1;
}
