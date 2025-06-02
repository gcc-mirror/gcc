/* { dg-additional-options -std=c++23 } */

/* numerics  */

#include <algorithm>
#include <numeric>
#include <ranges>
#include <span>
#include <vector>

//TODO PR120454 "C++ constexpr vs. OpenMP implicit mapping"
#pragma omp declare target(std::ranges::all_of, std::ranges::iota)

#include "target-flex-common.h"

namespace stdr = std::ranges;

bool test(std::size_t arg)
{
  bool ok;
  int midpoint_out;
  std::vector<int> vec(arg);
  int *data = vec.data();
  std::size_t size = vec.size();
  #pragma omp target defaultmap(none) map(from: ok, midpoint_out) map(tofrom: data[:size]) map(to: arg, size)
    {
      std::span span = {data, size};
      bool inner_ok = true;
      {
	VERIFY (stdr::all_of(span, [](int v){ return v == int{}; }));
	stdr::iota(span, 0);
	midpoint_out = *std::midpoint(span.data(), span.data() + span.size());
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (stdr::equal(vec, std::views::iota(0, static_cast<int>(vec.size()))));
  VERIFY_NON_TARGET (*std::midpoint(vec.data(), vec.data() + vec.size())
		     == midpoint_out);
  return true;
}

int main()
{
  volatile std::size_t arg = 42;
  return test(arg) ? 0 : 1;
}
