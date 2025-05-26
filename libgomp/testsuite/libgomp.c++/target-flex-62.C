/* { dg-additional-options -std=c++23 } */

/* std::views stuff.  Also tests std::tuple with std::views::zip.  */

#include <algorithm>
#include <ranges>
#include <span>

//TODO PR120454 "C++ constexpr vs. OpenMP implicit mapping"
#pragma omp declare target(std::ranges::all_of, std::ranges::equal, std::ranges::fold_left, std::views::reverse, std::views::zip)

#include "target-flex-common.h"

namespace stdr = std::ranges;
namespace stdv = std::views;

bool f()
{
  const int arr_fwd[8] = {0, 1, 2, 3, 4, 5, 6, 7};
  const int arr_rev[8] = {7, 6, 5, 4, 3, 2, 1, 0};

  bool ok;
  #pragma omp target defaultmap(none) map(from: ok) map(to: arr_fwd[:8], arr_rev[:8])
    {
      std::span<const int> fwd = {arr_fwd, 8};
      std::span<const int> rev = {arr_rev, 8};
      bool inner_ok = true;
      {
	VERIFY(stdr::equal(fwd, rev | stdv::reverse));
	VERIFY(stdr::equal(fwd | stdv::drop(4) | stdv::reverse,
			   rev | stdv::take(4)));
	for (auto [first, second] : stdv::zip(fwd, rev))
	  VERIFY(first + second == 7);
	auto plus = [](int a, int b){ return a + b; };
	auto is_even = [](int v){ return v % 2 == 0; };
	VERIFY(stdr::fold_left(fwd | stdv::filter(is_even), 0, plus)
	       == 12);
	VERIFY(stdr::all_of(fwd | stdv::transform([](int v){ return v * 2; }),
			    is_even));
      }
      end:
      ok = inner_ok;
    }
  return ok;
}

int main()
{
  return f() ? 0 : 1;
}
