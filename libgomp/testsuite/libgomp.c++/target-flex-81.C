/* { dg-additional-options "-std=c++20" } */

#include <ranges>
#include <span>
#include <type_traits>
#include <vector>

#include "target-flex-common.h"

namespace stdr = std::ranges;

template<typename It0, typename It1>
bool simple_equal(It0 it0, const It0 end0,
		  It1 it1, const It1 end1) noexcept
{
  for (; it0 != end0; ++it0, ++it1)
    if (it1 == end1 || *it0 != *it1)
      return false;
  return true;
}

template<typename Rn0, typename Rn1>
bool simple_equal(Rn0&& rn0, Rn1&& rn1) noexcept
{
  return simple_equal(stdr::begin(rn0), stdr::end(rn0),
		      stdr::begin(rn1), stdr::end(rn1));
}

template<typename Rn>
bool test(Rn&& range)
{
  using value_type = stdr::range_value_t<std::remove_cvref_t<Rn>>;
  std::vector<value_type> vec = {stdr::begin(range), stdr::end(range)};
  value_type *data = vec.data();
  std::size_t size = vec.size();
  bool ok;
  #pragma omp target map(from: ok) map(tofrom: data[:size]) map(to: size)
    {
      std::vector<value_type> orig = {data, data + size};
      std::span<value_type> span = {data, size};
      bool inner_ok = true;
      {
	auto mul_by_2 = [](const value_type& v){ return v * 2; };
	VERIFY (simple_equal(orig, span));
	for (auto& elem : span)
	  elem = mul_by_2(elem);
	VERIFY (simple_equal(orig | std::views::transform(mul_by_2), span));
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  auto mul_by_2 = [](const value_type& v){ return v * 2; };
  VERIFY_NON_TARGET (simple_equal(range | std::views::transform(mul_by_2), vec));
  return true;
}

struct my_int
{
  int _v;
  bool operator==(my_int const&) const = default;
  my_int operator*(int rhs) const noexcept {
    return {_v * rhs};
  }
};

int main()
{
  std::vector<int> ints = {1, 2, 3, 4, 5};
  const bool ints_res = test(ints);
  std::vector<my_int> my_ints = {my_int{1}, my_int{2}, my_int{3}, my_int{4}, my_int{5}};
  const bool my_ints_res = test(my_ints);
  return ints_res && my_ints_res ? 0 : 1;
}
