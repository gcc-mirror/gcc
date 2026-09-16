// { dg-do run { target c++11 } }
// { dg-additional-options "-ffloat-store" { target { m68*-*-* || ia32 } } }
// { dg-require-cstdint "" }

#include <random>
#include <testsuite_hooks.h>

template<typename Distribution>
void
validate(Distribution&& source)
{
  const auto& params = source.param();

  Distribution constr(params);
  VERIFY( constr == source );

  Distribution setter;
  setter.param(params);
  VERIFY( setter == source );

  std::mt19937_64 e1(0x19650809ull), e2 = e1, e3 = e1;
  for (int i = 0; i < 10; ++i)
  {
    const auto expected = source(e1);
    VERIFY( constr(e2) == expected );
    VERIFY( setter(e3) == expected );
  }
}

int main()
{
  // PR83833, PR123409
  validate(std::uniform_int_distribution<>(13, 22));
  validate(std::uniform_real_distribution<>(1.0, 9.0));

  validate(std::bernoulli_distribution(0.478));
  validate(std::binomial_distribution<>(7, 0.334));
  validate(std::negative_binomial_distribution<>(726, 0.65));
  validate(std::geometric_distribution<>(0.545));

  validate(std::poisson_distribution<>(6));
  validate(std::exponential_distribution<>(3));
  validate(std::gamma_distribution<>(1.1, 2.2));
  validate(std::weibull_distribution<>(1.7, 3.0));
  validate(std::extreme_value_distribution<>(-1.628, 1.628));

  validate(std::normal_distribution<>(5.0, 2.0));
  validate(std::lognormal_distribution<>(1.67, 0.25));
  validate(std::chi_squared_distribution<>(5));
  validate(std::cauchy_distribution<>(-3, 0.25));
  validate(std::fisher_f_distribution<>(26.0, 7.0));
  validate(std::student_t_distribution<>(65.0));

  const auto wf = [](double x) { return 1 + x; };
  validate(std::discrete_distribution<>(4, 1.2, 4.5, wf));
  validate(std::piecewise_constant_distribution<>(5, 0.5, 9, wf));
  validate(std::piecewise_linear_distribution<>(3, 1.2, 3.4, wf));

  return 0;
}
