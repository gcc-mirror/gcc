/* { dg-additional-options "-std=c++20" } */

/* <iterator> c++20  */

/* std::common_iterator uses std::variant.  */

#include <vector>
#include <iterator>
#include <span>

//TODO PR120454 "C++ constexpr vs. OpenMP implicit mapping"
#pragma omp declare target(std::ranges::distance, std::ranges::next)

#include "target-flex-common.h"

namespace stdr = std::ranges;

template<typename It0, typename It1>
bool simple_equal(const It0 begin0, const It0 end0,
		  const It1 begin1, const It1 end1) BL_NOEXCEPT
{
  It0 it0 = begin0;
  It1 it1 = begin1;
  for (; it0 != end0; ++it0, ++it1)
    if (it1 == end1 || *it0 != *it1)
      return false;
  return true;
}

template<typename It, typename OutIt>
void simple_copy(const It begin, const It end, OutIt out) BL_NOEXCEPT
{
  for (It it = begin; it != end; ++it, ++out)
    *out = *it;
}

template<typename T, std::size_t Size>
bool test(const T (&arr)[Size])
{
  bool ok;
  T out_rev_arr[Size];
  T out_fwd_arr[Size];
  T out_first_half_arr[Size / 2];
  #pragma omp target defaultmap(none) \
		     map(from: ok, out_rev_arr[:Size], out_fwd_arr[:Size], \
			       out_first_half_arr[:Size / 2]) \
		     map(to: arr[:Size])
    {
      bool inner_ok = true;
      {
	std::span<const T> span = {arr, Size};
	std::vector<T> rev_vec(std::reverse_iterator{span.end()},
			       std::reverse_iterator{span.begin()});
	VERIFY (std::distance(span.begin(), span.end())
		== std::distance(rev_vec.begin(), rev_vec.end()));
	VERIFY (stdr::distance(span.begin(), span.end())
		== stdr::distance(rev_vec.begin(), rev_vec.end()));
	VERIFY (stdr::distance(span) == stdr::distance(rev_vec));
	VERIFY (simple_equal(span.begin(), span.end(),
			     std::reverse_iterator{rev_vec.end()},
			     std::reverse_iterator{rev_vec.begin()}));
	simple_copy(rev_vec.begin(), rev_vec.end(), out_rev_arr);
	simple_copy(std::reverse_iterator{rev_vec.end()},
		    std::reverse_iterator{rev_vec.begin()},
		    out_fwd_arr);
	using counted_iter = std::counted_iterator<decltype(span.begin())>;
	using common_iter = std::common_iterator<counted_iter,
						 std::default_sentinel_t>;
	std::vector<T> front_half;
	simple_copy(common_iter{counted_iter{span.begin(), Size / 2}},
		    common_iter{std::default_sentinel},
		    std::back_insert_iterator{front_half});
	VERIFY (simple_equal(span.begin(), stdr::next(span.begin(), Size / 2),
			     front_half.begin(), front_half.end()));
	simple_copy(front_half.begin(), front_half.end(), out_first_half_arr);
      }
      end:
      ok = inner_ok;
    }
  VERIFY_NON_TARGET (simple_equal(std::reverse_iterator{arr + Size},
				  std::reverse_iterator{arr},
				  out_rev_arr, out_rev_arr + Size));
  VERIFY_NON_TARGET (simple_equal(arr, arr + Size,
				  out_fwd_arr, out_fwd_arr + Size));
  VERIFY_NON_TARGET (simple_equal(arr, arr + Size / 2,
				  out_first_half_arr, out_first_half_arr + Size / 2));
  return ok;
}

int main()
{
  int arr[] = {0, 1, 2, 3, 4, 5, 6, 7};
  return test(arr) ? 0 : 1;
}
