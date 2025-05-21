// { dg-do run }
// { dg-additional-options "-std=c++20" }

#include <cmath>
#include <complex>
#include <numbers>

using namespace std::complex_literals;

#define FP_EQUAL(x,y) (std::abs ((x) - (y)) < 1E-6)
#define COMPLEX_EQUAL(x,y) (FP_EQUAL ((x).real (), (y).real ()) \
			    && FP_EQUAL ((x).imag (), (y).imag ()))

#pragma omp declare target
template<typename T> bool test_complex ()
{
  std::complex<T> z (-1.334, 5.763);

  if (!FP_EQUAL (z.real (), (T) -1.334))
    return false;
  if (!FP_EQUAL (z.imag (), (T) 5.763))
    return false;
  if (!FP_EQUAL (std::abs (z),
		 std::sqrt (z.real () * z.real () + z.imag () * z.imag ())))
    return false;
  if (!FP_EQUAL (std::arg (z), std::atan2 (z.imag (), z.real ())))
    return false;
  if (!FP_EQUAL (std::norm (z), z.real () * z.real () + z.imag () * z.imag ()))
    return false;

  auto conj = std::conj (z);
  if (!FP_EQUAL (conj.real (), z.real ())
      || !FP_EQUAL (conj.imag (), -z.imag ()))
    return false;

  if (std::proj (z) != z)
    return false;

  auto infz1 = std::proj (std::complex<float> (INFINITY, -1));
  if (infz1.real () != INFINITY || infz1.imag () != (T) -0.0)
    return false;
  auto infz2 = std::proj (std::complex<float> (0, -INFINITY));
  if (infz2.real () != INFINITY || infz2.imag () != (T) -0.0)
    return false;

  auto polarz = std::polar ((T) 1.5, std::numbers::pi_v<T> / 4);
  if (!FP_EQUAL (polarz.real (), (T) 1.5 * std::cos (std::numbers::pi_v<T> / 4))
      || !FP_EQUAL (polarz.imag (),
		    (T) 1.5* std::sin (std::numbers::pi_v<T> / 4)))
    return false;

  return true;
}

template<typename T> bool test_complex_exp_log ()
{
  std::complex<T> z (-1.724, -3.763);

  // Euler's identity
  auto eulerz = std::exp (std::complex<T> (0, std::numbers::pi));
  eulerz += 1.0;
  if (!COMPLEX_EQUAL (eulerz, std::complex<T> ()))
    return false;

  auto my_exp_z
    = std::complex<T> (std::exp (z.real ()) * std::cos (z.imag ()),
		       std::exp (z.real ()) * std::sin (z.imag ()));
  if (!COMPLEX_EQUAL (std::exp (z), my_exp_z))
    return false;

  if (!COMPLEX_EQUAL (std::log10 (z),
		      std::log (z) / std::log (std::complex<T> (10))))
    return false;

  return true;
}

template<typename T> bool test_complex_trig ()
{
  std::complex<T> z (std::numbers::pi / 8, std::numbers::pi / 10);
  const std::complex<T> i (0, 1);

  auto my_sin_z
    = std::complex<T> (std::sin (z.real ()) * std::cosh (z.imag ()),
		       std::cos (z.real ()) * std::sinh (z.imag ()));
  if (!COMPLEX_EQUAL (std::sin (z), my_sin_z))
    return false;

  auto my_cos_z
    = std::complex<T> (std::cos (z.real ()) * std::cosh (z.imag ()),
		       -std::sin (z.real ()) * std::sinh (z.imag ()));
  if (!COMPLEX_EQUAL (std::cos (z), my_cos_z))
    return false;

  auto my_tan_z
    = std::complex<T> (std::sin (2*z.real ()), std::sinh (2*z.imag ()))
      / (std::cos (2*z.real ()) + std::cosh (2*z.imag ()));
  if (!COMPLEX_EQUAL (std::tan (z), my_tan_z))
    return false;

  auto my_sinh_z
    = std::complex<T> (std::sinh (z.real ()) * std::cos (z.imag ()),
		       std::cosh (z.real ()) * std::sin (z.imag ()));
  if (!COMPLEX_EQUAL (std::sinh (z), my_sinh_z))
    return false;

  auto my_cosh_z
    = std::complex<T> (std::cosh (z.real ()) * std::cos (z.imag ()),
		       std::sinh (z.real ()) * std::sin (z.imag ()));
  if (!COMPLEX_EQUAL (std::cosh (z), my_cosh_z))
    return false;

  auto my_tanh_z
    = std::complex<T> (std::sinh (2*z.real ()),
		       std::sin (2*z.imag ()))
		       / (std::cosh (2*z.real ()) + std::cos (2*z.imag ()));
  if (!COMPLEX_EQUAL (std::tanh (z), my_tanh_z))
    return false;

  auto my_asin_z = -i * std::log (i * z + std::sqrt ((T) 1.0 - z*z));
  if (!COMPLEX_EQUAL (std::asin (z), my_asin_z))
    return false;

  auto my_acos_z
    = std::complex<T> (std::numbers::pi / 2)
		       + i * std::log (i * z + std::sqrt ((T) 1.0 - z*z));
  if (!COMPLEX_EQUAL (std::acos (z), my_acos_z))
    return false;

  auto my_atan_z = std::complex<T> (0, -0.5) * (std::log ((i - z) / (i + z)));
  if (!COMPLEX_EQUAL (std::atan (z), my_atan_z))
    return false;

  auto my_asinh_z = std::log (z + std::sqrt (z*z + (T) 1.0));
  if (!COMPLEX_EQUAL (std::asinh (z), my_asinh_z))
    return false;

  auto my_acosh_z = std::log (z + std::sqrt (z*z - (T) 1.0));
  if (!COMPLEX_EQUAL (std::acosh (z), my_acosh_z))
    return false;

  auto my_atanh_z
    = std::complex<T> (0.5) * (std::log ((T) 1.0 + z) - std::log ((T) 1.0 - z));
  if (!COMPLEX_EQUAL (std::atanh (z), my_atanh_z))
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

      RUN_TEST (complex);
      RUN_TEST (complex_exp_log);
      RUN_TEST (complex_trig);
    } while (false);

  return result;
}
