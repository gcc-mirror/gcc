// { dg-do run }
// { dg-additional-options -DMEM_SHARED { target offload_device_shared_as } }

#include <stdlib.h>
#include <time.h>
#include <omp.h>
#include <forward_list>
#include <algorithm>

#define N 3000

void init (int data[])
{
  for (int i = 0; i < N; ++i)
    data[i] = rand ();
}

#pragma omp declare target
bool validate (const std::forward_list<int> &list, int data[])
{
  int i = 0;
  for (auto &v : list)
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
  std::forward_list<int> list (std::begin (data), std::end (data));
#else
  std::forward_list<int> list;
#endif

#ifndef MEM_SHARED
  #pragma omp target data map (to: data[:N]) map (alloc: list)
#endif
    {
#ifndef MEM_SHARED
      #pragma omp target
	new (&list) std::forward_list<int> (std::begin (data), std::end (data));
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
	    auto it = list.begin ();
	    std::advance (it, start);
	    for (int i = 0; i < len; ++i)
	      {
		*it *= *it;
		++it;
	      }
	  } while (false);

      #pragma omp target map (from: ok)
	{
	  ok = validate (list, data);
#ifndef MEM_SHARED
	  list.~forward_list ();
#endif
	}
    }

  return ok ? 0 : 1;
}
