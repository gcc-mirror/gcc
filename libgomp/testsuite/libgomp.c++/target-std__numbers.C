// { dg-do run }
// { dg-additional-options "-std=c++20" }

#include <cmath>
#include <numbers>

#define FP_EQUAL(x,y) (std::abs ((x) - (y)) < 1E-6)

#pragma omp declare target
template<typename T> bool test_pi ()
{
  if (!FP_EQUAL (std::sin (std::numbers::pi_v<T>), (T) 0.0))
    return false;
  if (!FP_EQUAL (std::cos (std::numbers::pi_v<T>), (T) -1.0))
    return false;
  if (!FP_EQUAL (std::numbers::pi_v<T> * std::numbers::inv_pi_v<T>, (T) 1.0))
    return false;
  if (!FP_EQUAL (std::numbers::pi_v<T> * std::numbers::inv_sqrtpi_v<T>
		 * std::numbers::inv_sqrtpi_v<T>, (T) 1.0))
    return false;
  return true;
}

template<typename T> bool test_sqrt ()
{
  if (!FP_EQUAL (std::numbers::sqrt2_v<T> * std::numbers::sqrt2_v<T>, (T) 2.0))
    return false;
  if (!FP_EQUAL (std::numbers::sqrt3_v<T> * std::numbers::sqrt3_v<T>, (T) 3.0))
    return false;
  return true;
}

template<typename T> bool test_phi ()
{
  T myphi = ((T) 1.0 + std::sqrt ((T) 5.0)) / (T) 2.0;
  if (!FP_EQUAL (myphi, std::numbers::phi_v<T>))
    return false;
  return true;
}

template<typename T> bool test_log ()
{
  if (!FP_EQUAL (std::log ((T) 2.0), std::numbers::ln2_v<T>))
    return false;
  if (!FP_EQUAL (std::log ((T) 10.0), std::numbers::ln10_v<T>))
    return false;
  if (!FP_EQUAL (std::log2 ((T) std::numbers::e), std::numbers::log2e_v<T>))
    return false;
  if (!FP_EQUAL (std::log10 ((T) std::numbers::e), std::numbers::log10e_v<T>))
    return false;
  return true;
}

template<typename T> bool test_egamma ()
{
  T myegamma = 0.0;
  #pragma omp parallel for reduction(+:myegamma)
    for (int k = 2; k < 100000; ++k)
      myegamma += (std::riemann_zeta (k) - 1) / k;
  myegamma = (T) 1 - myegamma;
  if (!FP_EQUAL (myegamma, std::numbers::egamma_v<T>))
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

      RUN_TEST (pi);
      RUN_TEST (sqrt);
      RUN_TEST (phi);
      RUN_TEST (log);
      RUN_TEST (egamma);
    } while (false);

  return result;
}
