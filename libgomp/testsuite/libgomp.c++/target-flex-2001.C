/* { dg-additional-options "-std=c++20" } */

/* Functional  */

#include <functional>
#include <utility>

#include "target-flex-common.h"

template<typename T,typename Fn>
auto invoke_unary(T&& a, Fn&& fn) noexcept
{
  return std::invoke(std::forward<Fn>(fn),
		     std::forward<T>(a));
}

template<typename T, typename U, typename Fn>
auto invoke_binary(T&& a, U&& b, Fn&& fn) noexcept
{
  return std::invoke(std::forward<Fn>(fn),
		     std::forward<T>(a),
		     std::forward<U>(b));
}

bool test(unsigned arg)
{
  bool ok;
  #pragma omp target map(from: ok) map(to: arg)
    {
      bool inner_ok = true;
      {
	VERIFY (std::plus{}(arg, 2) == arg + 2);
	auto bound_plus_arg = std::bind_front(std::plus{}, arg);
	VERIFY (bound_plus_arg(10) == arg + 10);
	VERIFY (bound_plus_arg(20) == arg + 20);

	VERIFY (std::not_fn(std::not_equal_to{})(arg, arg));
	VERIFY (invoke_binary(arg, arg, std::not_fn(std::not_equal_to{})));
	auto bound_equals_arg = std::bind_front(std::not_fn(std::not_equal_to{}), arg);
	VERIFY (bound_equals_arg(arg));
	VERIFY (std::not_fn(bound_equals_arg)(arg + 1));
	VERIFY (invoke_unary(arg, bound_equals_arg));

	VERIFY (std::not_fn(std::ranges::not_equal_to{})(arg, arg));
	VERIFY (invoke_binary(arg, arg, std::not_fn(std::ranges::not_equal_to{})));
	auto bound_ranges_equals_arg = std::bind_front(std::not_fn(std::ranges::not_equal_to{}), arg);
	VERIFY (bound_ranges_equals_arg(arg));
	VERIFY (std::not_fn(bound_ranges_equals_arg)(arg + 1));
	VERIFY (invoke_unary(arg, bound_ranges_equals_arg));
      }
      end:
      ok = inner_ok;
    }
  return ok;
}

int main()
{
  volatile unsigned arg = 42u;
  return test(arg) ? 0 : 1;
}
