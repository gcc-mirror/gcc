/* { dg-additional-options "-std=c++20" } */

/* ranged algorithms c++20 */

#include <algorithm>
#include <ranges>
#include <vector>

//TODO PR120454 "C++ constexpr vs. OpenMP implicit mapping"
#pragma omp declare target(std::ranges::copy, std::ranges::equal, std::ranges::rotate, std::ranges::transform)

#include "target-flex-common.h"

namespace stdr = std::ranges;

template<typename T, std::size_t Size>
bool test(const T (&arr)[Size])
{
  bool ok;
  T out_2x_arr[Size];
  T out_shifted_arr[Size];
  #pragma omp target defaultmap(none) \
		     map(from: ok, out_2x_arr[:Size], out_shifted_arr[:Size]) \
		     map(to: arr[:Size])
    {
      std::vector<T> vec(Size);
      std::vector<T> mutated(Size);
      bool inner_ok = true;
      {
	stdr::copy(arr, vec.begin());
	VERIFY (stdr::equal(arr, vec));
	stdr::transform(vec, mutated.begin(),
			[](const T& v){ return v * 2; });
	stdr::copy(mutated, out_2x_arr);
	stdr::rotate(vec, std::next(vec.begin(), Size / 2));
	stdr::copy(vec, out_shifted_arr);
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (stdr::equal(arr, out_2x_arr, stdr::equal_to{}, [](const T& v){ return v * 2; }));
  std::vector<T> shifted(arr, arr + Size);
  stdr::rotate(shifted, std::next(shifted.begin(), Size / 2));
  VERIFY_NON_TARGET (stdr::equal(out_shifted_arr, shifted));
  return true;
}

int main()
{
  int arr[] = {0, 1, 2, 3, 4, 5, 6, 7};
  return test(arr) ? 0 : 1;
}
