// { dg-do run }
// { dg-additional-options -DMEM_SHARED { target offload_device_shared_as } }

#include <stdlib.h>
#include <time.h>
#include <omp.h>
#include <list>
#include <algorithm>

#define N 3000

void init (int data[])
{
  for (int i = 0; i < N; ++i)
    data[i] = rand ();
}

#pragma omp declare target
bool validate (const std::list<int> &_list, int data[])
{
  int i = 0;
  for (auto &v : _list)
    {
      if (v != data[i] * data[i])
	return false;
      ++i;
    }
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
  std::list<int> _list (std::begin (data), std::end (data));
#else
  std::list<int> _list;
#endif

#ifndef MEM_SHARED
  #pragma omp target data map (to: data[:N]) map (alloc: _list)
#endif
    {
#ifndef MEM_SHARED
      #pragma omp target
	new (&_list) std::list<int> (std::begin (data), std::end (data));
#endif

      #pragma omp target teams
	do
	  {
	    int len = N / omp_get_num_teams () + (N % omp_get_num_teams () > 0);
	    int start = len * omp_get_team_num ();
	    if (start >= N)
	      break;
	    if (start + len >= N)
	      len = N - start;
	    auto it = _list.begin ();
	    std::advance (it, start);
	    for (int i = 0; i < len; ++i)
	      {
		*it *= *it;
		++it;
	      }
	  } while (false);

      #pragma omp target map (from: ok)
	{
	  ok = validate (_list, data);
#ifndef MEM_SHARED
	  _list.~list ();
#endif
	}
    }

  return ok ? 0 : 1;
}
