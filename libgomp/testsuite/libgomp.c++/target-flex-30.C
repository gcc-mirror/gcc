/* std::initializer_list in target region.  */

#include <initializer_list>
#include <array>

#include "target-flex-common.h"

bool test_initializer_list(int arg)
{
  static constexpr std::size_t out_arr_size = 7;
  int out_arr[out_arr_size];
  bool ok;
  #pragma omp target map(from: ok, out_arr[:out_arr_size]) map(to: arg)
    {
      bool inner_ok = true;
      {
	auto il = {0, 1, 2, 3, 4, 5, arg};

	int sum = 0;
	for (auto const& e : il)
	  sum += e;
	VERIFY (sum == 0 + 1 + 2 + 3 + 4 + 5 + arg);

	auto* out_it = out_arr;
	const auto* const out_end = out_arr + out_arr_size;
	for (auto const& e : il)
	  {
	    VERIFY (out_it != out_end);
	    *out_it = e;
	    ++out_it;
	  }
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;

  std::array<int, out_arr_size> reference_array = {0, 1, 2, 3, 4, 5, arg};
  const auto *out_arr_it = out_arr;
  for (auto const& e : reference_array)
    VERIFY_NON_TARGET (e == *(out_arr_it++));

  return true;
}

int main()
{
  volatile int arg = 42;
  return test_initializer_list(arg) ? 0 : 1;
}
