/* algorithms pre c++20 */

#include <algorithm>
#include <vector>

#include "target-flex-common.h"

template<typename T, std::size_t Size>
bool test(const T (&arr)[Size])
{
  bool ok;
  T out_2x_arr[Size];
  T out_shifted_arr[Size];
  #pragma omp target map(from: ok, out_2x_arr[:Size], out_shifted_arr[:Size]) \
		     map(to: arr[:Size])
    {
      std::vector<T> vec(Size);
      std::vector<T> mutated(Size);
      bool inner_ok = true;
      {
	std::copy(arr, arr + Size, vec.begin());
	VERIFY (std::equal(arr, arr + Size, vec.begin()));
	std::transform(vec.begin(), vec.end(), mutated.begin(),
		       [](const T& v){ return v * 2; });
	std::copy(mutated.begin(), mutated.end(), out_2x_arr);
	std::rotate(vec.begin(), std::next(vec.begin(), Size / 2), vec.end());
	std::copy(vec.begin(), vec.end(), out_shifted_arr);
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (std::equal(arr, arr + Size, out_2x_arr,
				[](const T& a, const T& b){ return a * 2 == b; }));
  std::vector<T> shifted(arr, arr + Size);
  std::rotate(shifted.begin(), std::next(shifted.begin(), Size / 2), shifted.end());
  VERIFY_NON_TARGET (std::equal(out_shifted_arr, out_shifted_arr + Size, shifted.begin()));
  return true;
}

int main()
{
  int arr[] = {0, 1, 2, 3, 4, 5, 6, 7};
  return test(arr) ? 0 : 1;
}
