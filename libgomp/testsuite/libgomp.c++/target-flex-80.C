// { dg-additional-options "-std=c++20" }

/* std::span  */

#include <span>

#include "target-flex-common.h"

template<typename It0, typename It1>
bool simple_equal(It0 it0, const It0 end0,
		  It1 it1, const It1 end1) noexcept
{
  for (; it0 != end0; ++it0, ++it1)
    if (it1 == end1 || *it0 != *it1)
      return false;
  return true;
}

template<typename T, std::size_t Size>
bool test(const T (&arr)[Size])
{
  bool ok;
  T out_arr[Size];
  #pragma omp target map(from: ok) map(to: arr[:Size])
    {
      std::span span = {arr, Size};
      bool inner_ok = true;
      {
	VERIFY (!span.empty());
	VERIFY (span.size() == Size);
	auto out_it = out_arr;
	for (auto elem : span)
	  *(out_it++) = elem;
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (simple_equal(arr, arr + Size,
				  out_arr, out_arr + Size));
  return true;
}

int main()
{
  int arr[8] = {0, 1, 2, 3, 4, 5, 6, 7};
  return test(arr) ? 0 : 1;
}
