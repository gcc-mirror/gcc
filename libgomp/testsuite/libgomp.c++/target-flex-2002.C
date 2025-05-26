/* { dg-additional-options "-std=c++23" } */

/* expected/optional  */

#include <optional>
#include <expected>

#include "target-flex-common.h"

std::optional<unsigned> make_optional(bool b, unsigned arg = 0u) noexcept
{
  if (!b)
    return std::nullopt;
  return {arg};
}

bool test_optional(unsigned arg)
{
  bool ok;
  #pragma omp target map(from: ok) map(to: arg)
    {
      bool inner_ok = true;
      {
	auto null_opt = make_optional(false);
	VERIFY (!null_opt);
	VERIFY (!null_opt.has_value());
	VERIFY (null_opt.value_or(arg * 2u) == arg * 2u);
	VERIFY (null_opt.or_else([&](){ return std::optional<unsigned>{arg}; })
			.transform([](int a){ return a * 2u; })
			.value_or(0) == arg * 2u);

	auto opt = make_optional(true, arg);
	VERIFY (opt);
	VERIFY (opt.has_value());
	VERIFY (opt.value() == arg);
	VERIFY (*opt == arg);
	VERIFY (opt.value_or(arg + 42) == arg);
	VERIFY (opt.or_else([&](){ return std::optional<unsigned>{arg + 42}; })
		   .transform([](int a){ return a * 2u; })
		   .value_or(0) == arg * 2u);
      }
      end:
      ok = inner_ok;
    }
  return ok;
}

struct my_error
{
  int _e;
};

std::expected<unsigned, my_error> make_expected(bool b, unsigned arg = 0u) noexcept
{
  if (!b)
    return std::unexpected{my_error{-1}};
  return {arg};
}

bool test_expected(unsigned arg)
{
  bool ok;
  #pragma omp target map(from: ok) map(to: arg)
    {
      bool inner_ok = true;
      {
	auto unexpected = make_expected(false);
	VERIFY (!unexpected);
	VERIFY (!unexpected.has_value());
	VERIFY (unexpected.error()._e == -1);
	VERIFY (unexpected.value_or(arg * 2u) == arg * 2u);
	VERIFY (unexpected.or_else([&](my_error e){ return std::expected<unsigned, my_error>{arg}; })
			  .transform([](int a){ return a * 2u; })
			  .value_or(0) == arg * 2u);

	auto expected = make_expected(true, arg);
	VERIFY (expected);
	VERIFY (expected.has_value());
	VERIFY (expected.value() == arg);
	VERIFY (*expected == arg);
	VERIFY (expected.value_or(arg + 42) == arg);
	VERIFY (expected.or_else([&](my_error e){ return std::expected<unsigned, my_error>{std::unexpected{e}}; })
			.transform([](int a){ return a * 2u; })
			.value_or(0) == arg * 2u);
      }
      end:
      ok = inner_ok;
    }
  return ok;
}

int main()
{
  volatile unsigned arg = 42;
  return test_optional(arg)
	 && test_expected(arg) ? 0 : 1;
}
