// { dg-do run }
// { dg-additional-options "-std=c++20" }

#include <cmath>
#include <numbers>

#define FP_EQUAL(x,y) (std::abs ((x) - (y)) < 1E-6)

#pragma omp declare target
template<typename T> bool test_basic ()
{
  T x = -3.456789;
  T y = 1.234567;
  T z = 5.678901;

  if (std::abs (x) != -x)
    return false;
  if (!FP_EQUAL (std::trunc (x / y) * y + std::fmod (x, y), x))
    return false;
  if (!FP_EQUAL (x - std::round (x / y) * y, std::remainder (x, y)))
    return false;
  if (!FP_EQUAL (std::fma (x, y, z), x * y + z))
    return false;
  if (std::fmax (x, y) != (x > y ? x : y))
    return false;
  if (std::fmin (x, y) != (x < y ? x : y))
    return false;
  if (std::fdim (x, y) != std::max(x - y, (T) 0.0))
    return false;
  if (std::fdim (y, x) != std::max(y - x, (T) 0.0))
    return false;
  return true;
}

template<typename T> bool test_exp ()
{
  T x = -4.567890;
  T y = 2.345678;

  if (!FP_EQUAL (std::exp (x), std::pow (std::numbers::e_v<T>, x)))
    return false;
  if (!FP_EQUAL (std::exp2 (y), std::pow ((T) 2.0, y)))
    return false;
  if (!FP_EQUAL (std::expm1 (y), std::exp (y) - (T) 1.0))
    return false;
  if (!FP_EQUAL (std::log (std::exp (x)), x))
    return false;
  if (!FP_EQUAL (std::log10 (std::pow ((T) 10.0, y)), y))
    return false;
  if (!FP_EQUAL (std::log2 (std::exp2 (y)), y))
    return false;
  if (!FP_EQUAL (std::log1p (std::expm1 (y)), y))
    return false;
  return true;
}

template<typename T> bool test_power ()
{
  T x = 7.234251;
  T y = 0.340128;

  if (!FP_EQUAL (std::log (std::pow (x, y)) / std::log (x), y))
    return false;
  if (!FP_EQUAL (std::sqrt (x) * std::sqrt (x), x))
    return false;
  if (!FP_EQUAL (std::cbrt (x) * std::cbrt (x) * std::cbrt (x), x))
    return false;
  if (!FP_EQUAL (std::hypot (x, y), std::sqrt (x * x + y * y)))
    return false;
  return true;
}

template<typename T> bool test_trig ()
{
  T theta = std::numbers::pi / 4;
  T phi = std::numbers::pi / 6;

  if (!FP_EQUAL (std::sin (theta), std::sqrt ((T) 2) / 2))
    return false;
  if (!FP_EQUAL (std::sin (phi), 0.5))
    return false;
  if (!FP_EQUAL (std::cos (theta), std::sqrt ((T) 2) / 2))
    return false;
  if (!FP_EQUAL (std::cos (phi), std::sqrt ((T) 3) / 2))
    return false;
  if (!FP_EQUAL (std::tan (theta), 1.0))
    return false;
  if (!FP_EQUAL (std::tan (phi), std::sqrt ((T) 3) / 3))
    return false;

  T x = 0.33245623;

  if (!FP_EQUAL (std::asin (std::sin (x)), x))
    return false;
  if (!FP_EQUAL (std::acos (std::cos (x)), x))
    return false;
  if (!FP_EQUAL (std::atan (std::tan (x)), x))
    return false;
  if (!FP_EQUAL (std::atan2 (std::sin (x), std::cos (x)), x))
    return false;
  return true;
}

template<typename T> bool test_hyperbolic ()
{
  T x = 0.7423532;

  if (!FP_EQUAL (std::sinh (x), (std::exp (x) - std::exp (-x)) / (T) 2.0))
    return false;
  if (!FP_EQUAL (std::cosh (x), (std::exp (x) + std::exp (-x)) / (T) 2.0))
    return false;
  if (!FP_EQUAL (std::tanh (x), std::sinh (x) / std::cosh (x)))
    return false;
  if (!FP_EQUAL (std::asinh (std::sinh (x)), x))
    return false;
  if (!FP_EQUAL (std::acosh (std::cosh (x)), x))
    return false;
  if (!FP_EQUAL (std::atanh (std::tanh (x)), x))
    return false;
  return true;
}

template<typename T> bool test_erf ()
{
  if (!FP_EQUAL (std::erf ((T) 0), 0))
    return false;
  if (!FP_EQUAL (std::erf ((T) INFINITY), 1))
    return false;
  if (!FP_EQUAL (std::erf ((T) -INFINITY), -1))
    return false;

  if (!FP_EQUAL (std::erfc (0), 1))
    return false;
  if (!FP_EQUAL (std::erfc ((T) INFINITY), 0))
    return false;
  if (!FP_EQUAL (std::erfc ((T) -INFINITY), 2))
    return false;

  return true;
}

template<typename T> bool test_gamma ()
{
  if (!FP_EQUAL (std::tgamma ((T) 5), 4*3*2*1))
    return false;
  if (!FP_EQUAL (std::tgamma ((T) 0.5), std::sqrt (std::numbers::pi_v<T>)))
    return false;
  if (!FP_EQUAL (std::tgamma ((T) -0.5), (T) -2 * std::sqrt (std::numbers::pi_v<T>)))
    return false;
  if (!FP_EQUAL (std::tgamma ((T) 2.5), (T) 0.75 * std::sqrt (std::numbers::pi_v<T>)))
    return false;
  if (!FP_EQUAL (std::tgamma ((T) -2.5), (T) -8.0/15 * std::sqrt (std::numbers::pi_v<T>)))
    return false;

  if (!FP_EQUAL (std::lgamma ((T) 5), std::log ((T) 4*3*2*1)))
    return false;
  if (!FP_EQUAL (std::lgamma ((T) 0.5), std::log (std::sqrt (std::numbers::pi_v<T>))))
    return false;
  if (!FP_EQUAL (std::lgamma ((T) 2.5),
		 std::log ((T) 0.75 * std::sqrt (std::numbers::pi_v<T>))))
    return false;

  return true;
}

template<typename T> bool test_rounding ()
{
  T x = -2.5678;
  T y = 3.6789;

  if (std::ceil (x) != -2)
    return false;
  if (std::floor (x) != -3)
    return false;
  if (std::trunc (x) != -2)
    return false;
  if (std::round (x) != -3)
    return false;

  if (std::ceil (y) != 4)
    return false;
  if (std::floor (y) != 3)
    return false;
  if (std::trunc (y) != 3)
    return false;
  if (std::round (y) != 4)
    return false;

  /* Not testing std::rint and std::nearbyint due to dependence on
     floating-point environment.  */

  return true;
}

template<typename T> bool test_fpmanip ()
{
  T x = -2.3456789;
  T y = 3.6789012;
  int exp;

  T mantissa = std::frexp (x, &exp);
  if (std::ldexp (mantissa, exp) != x)
    return false;
  if (std::logb (x) + 1 != exp)
    return false;
  if (std::ilogb (x) + 1 != exp)
    return false;
  if (std::scalbn (x, -exp) != mantissa)
    return false;

  T next = std::nextafter (x, y);
  if (!(next > x && next < y))
    return false;

#if 0
  /* TODO Due to 'std::nexttoward' using 'long double to', this triggers a
     '80-bit-precision floating-point numbers unsupported (mode ‘XF’)' error
     with x86_64 host and nvptx, GCN offload compilers, or
     '128-bit-precision floating-point numbers unsupported (mode ‘TF’)' error
     with powerpc64le host and nvptx offload compiler, for example;
     PR71064 'nvptx offloading: "long double" data type'.
     It ought to work on systems where the host's 'long double' is the same as
     'double' ('DF'): aarch64, for example?  */
  next = std::nexttoward (x, y);
  if (!(next > x && next < y))
    return false;
#endif

  if (std::copysign (x, y) != std::abs (x))
    return false;
  if (std::copysign (y, x) != -y)
    return false;

  return true;
}

template<typename T> bool test_classify ()
{
  T x = -2.3456789;
  T y = 3.6789012;

  if (std::fpclassify (x) != FP_NORMAL || std::fpclassify (y) != FP_NORMAL)
    return false;
  if (std::fpclassify ((T) INFINITY) != FP_INFINITE
      || std::fpclassify ((T) -INFINITY) != FP_INFINITE)
    return false;
  if (std::fpclassify ((T) 0.0) != FP_ZERO)
    return false;
  if (std::fpclassify ((T) NAN) != FP_NAN)
    return false;
  if (!std::isfinite (x) || !std::isfinite (y))
    return false;
  if (std::isfinite ((T) INFINITY) || std::isfinite ((T) -INFINITY))
    return false;
  if (std::isinf (x) || std::isinf (y))
    return false;
  if (!std::isinf ((T) INFINITY) || !std::isinf ((T) -INFINITY))
    return false;
  if (std::isnan (x) || std::isnan (y))
    return false;
  if (!std::isnan ((T) 0.0 / (T) 0.0))
    return false;
  if (std::isnan (x) || std::isnan (y))
    return false;
  if (!std::isnormal (x) || !std::isnormal (y))
    return false;
  if (std::isnormal ((T) 0.0) || std::isnormal ((T) INFINITY) || std::isnormal ((T) NAN))
    return false;
  if (!std::signbit (x) || std::signbit (y))
    return false;

  return true;
}

template<typename T> bool test_compare ()
{
  T x = 5.6789012;
  T y = 8.9012345;

  if (std::isgreater (x, y))
    return false;
  if (std::isgreater (x, x))
    return false;
  if (std::isgreaterequal (x, y))
    return false;
  if (!std::isgreaterequal (x, x))
    return false;
  if (!std::isless (x, y))
    return false;
  if (std::isless (x, x))
    return false;
  if (!std::islessequal (x, y))
    return false;
  if (!std::islessequal (x, x))
    return false;
  if (!std::islessgreater (x, y))
    return false;
  if (std::islessgreater (x, x))
    return false;
  if (std::isunordered (x, y))
    return false;
  if (!std::isunordered (x, NAN))
    return false;
  return true;
}
#pragma omp end declare target

#define RUN_TEST(func) \
{ \
  pass++; \
  bool ok = test_##func<float> (); \
  if (!ok) { result = pass; break; } \
  pass++; \
  ok = test_##func<double> (); \
  if (!ok) { result = pass; break; } \
}

int main (void)
{
  int result = 0;

  #pragma omp target map (tofrom: result)
    do {
      int pass = 0;

      RUN_TEST (basic);
      RUN_TEST (exp);
      RUN_TEST (power);
      RUN_TEST (trig);
      RUN_TEST (hyperbolic);
      RUN_TEST (erf);
      RUN_TEST (gamma);
      RUN_TEST (rounding);
      RUN_TEST (fpmanip);
      RUN_TEST (classify);
      RUN_TEST (compare);
    } while (false);

  return result;
}
