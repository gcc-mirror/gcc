// { dg-do compile { target c++20 } }

#include <compare>
#include <limits>
#include <testsuite_hooks.h>

// Test floating-point ordering.

template<typename T> constexpr T epsilon = std::numeric_limits<T>::epsilon();

// PR middle-end/19779 IBM 128bit long double format is not constant folded.
// 1.0L + numeric_limits<long double>::epsilon() is not a constant expression
// so just use numeric_limits<double>::epsilon() instead.
#if defined __LONG_DOUBLE_IBM128__
using D = long double;
#elif defined __LONG_DOUBLE_IEEE128__ && defined __SIZEOF_IBM128__
using D = __ibm128;
#else
using D = double;
#endif

template<> constexpr D epsilon<D> = std::numeric_limits<double>::epsilon();

template<typename Float>
constexpr bool
test_fp()
{
  const auto& less = std::strong_ordering::less;
  const auto& greater = std::strong_ordering::greater;

  static_assert( std::numeric_limits<Float>::is_specialized);
  Float min = std::numeric_limits<Float>::lowest();
  Float max = std::numeric_limits<Float>::max();
  Float qnan = std::numeric_limits<Float>::quiet_NaN();
  Float snan = std::numeric_limits<Float>::signaling_NaN();
  Float inf = std::numeric_limits<Float>::infinity();
  Float denorm = std::numeric_limits<Float>::denorm_min();
  Float smallest = std::numeric_limits<Float>::min();

  // -qnan < -snan < -inf < -num < -zero < +zero < +num < +inf < +snan < +qnan

  struct Test
  {
    Float lhs;
    Float rhs;
    std::strong_ordering expected;

    constexpr explicit operator bool() const noexcept
    { return std::strong_order(lhs, rhs) == expected; }

    constexpr Test rev() const noexcept
    { return { rhs, lhs, 0 <=> expected }; }

    constexpr Test neg() const noexcept
    { return { -lhs, -rhs, 0 <=> expected }; }
  };

  auto test = [&](Test t, bool selftest = true) {
    if (selftest)
    {
      // Check that each operand compares equal to itself.
      if (std::strong_order(t.lhs, t.lhs) != std::strong_ordering::equal)
	return false;
      if (std::strong_order(t.rhs, t.rhs) != std::strong_ordering::equal)
	return false;
    }
    return t && t.rev() && t.neg() && t.rev().neg();
  };

  VERIFY(test({   1.0,  2.0, less    }));
  VERIFY(test({   1.0, -2.0, greater }));
  VERIFY(test({   3e6,  2e9, less    }));
  VERIFY(test({   8e8, -9e9, greater }));
  VERIFY(test({   0.0, -0.0, greater }));
  VERIFY(test({  -inf,  min, less    }));
  VERIFY(test({ -snan,  min, less    }));
  VERIFY(test({ -qnan,  min, less    }));

  const Float vals[] = {
    Float(0.0), denorm, smallest, Float(0.004), Float(0.2), Float(1.0),
    Float(1) + epsilon<Float>, Float(1.1), Float(1e3), Float(123e4), Float(1e9),
    max, inf, snan, qnan
  };

  for (auto& f : vals)
  {
    VERIFY(test({ f, -f, greater }));
    VERIFY(test({ f, min, greater }, false));
    for (auto& g : vals)
    {
      VERIFY(test({ f, g, &f <=> &g }, false));
      VERIFY(test({ f, -g, greater }, false));
    }
  }

  return true;
}

static_assert( test_fp<float>() );
static_assert( test_fp<double>() );
static_assert( test_fp<long double>() );
