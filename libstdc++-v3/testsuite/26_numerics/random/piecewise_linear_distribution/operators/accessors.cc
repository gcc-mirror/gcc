// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }

#include <random>
#include <testsuite_hooks.h>

template<typename RealType>
void
test_exact()
{
  RealType x[6] = {0.0, 1.0, 2.0, 3.0, 4.0, 5.0};
  RealType wt[6] = {0.0, 1.0, 1.0, 6.0, -1.0, 2.0};

  std::piecewise_linear_distribution<RealType>
    u(std::begin(x), std::end(x), wt);

  const std::vector<RealType>& interval = u.intervals();
  VERIFY( interval.size() == 6 );
  VERIFY( interval[0] == RealType(0.0) );
  VERIFY( interval[5] == RealType(5.0) );

#if _GLIBCXX_USE_NEW_PIECEWISE_DISTRIBUTIONS
  const std::vector<RealType>& density = u.densities();
#else
  const std::vector<double>& density = u.densities();
#endif

  VERIFY( density.size() == 6 );
  VERIFY( density[0] == RealType(0.0 / 8.0) );
  VERIFY( density[1] == RealType(1.0 / 8.0) );
  VERIFY( density[2] == RealType(1.0 / 8.0) );
  VERIFY( density[3] == RealType(6.0 / 8.0) );
  VERIFY( density[4] == RealType(-1.0 / 8.0) );
  VERIFY( density[5] == RealType(2.0 / 8.0) );
}

template<typename InputType, typename DistType = InputType>
void
test_precision_depended()
{
  constexpr bool preserved
#if _GLIBCXX_USE_NEW_PIECEWISE_DISTRIBUTIONS == 0
    = sizeof(InputType) <= sizeof(double);
#elif _GLIBCXX_USE_NEW_PIECEWISE_DISTRIBUTIONS == 2
    = sizeof(InputType) <= sizeof(DistType);
#else
    // input is converted to DistType and stored in double
    = (sizeof(InputType) <= sizeof(DistType))
      && (sizeof(InputType) <= sizeof(double));
#endif

  std::initializer_list<DistType> x{0.0, 0.5, 1.0};
  constexpr InputType step
    = std::numeric_limits<InputType>::epsilon() * 2;
  InputType wt[3]{InputType(1) - step, InputType(1), InputType(1) + step};

  using Distribution = std::piecewise_linear_distribution<DistType>;
  auto validate = [&wt](const Distribution& dist)
  {
    const std::vector<DistType>& interval = dist.intervals();
    VERIFY( interval.size() == 3 );
    VERIFY( interval[0] == DistType(0.0) );
    VERIFY( interval[2] == DistType(1.0) );

#if _GLIBCXX_USE_NEW_PIECEWISE_DISTRIBUTIONS
    const std::vector<DistType>& density = dist.densities();
#else
    const std::vector<double>& density = dist.densities();
#endif

    VERIFY( density.size() == 3 );
    VERIFY( density[0] == (preserved ? wt[0] : InputType(1)) );
    VERIFY( density[1] == InputType(1) );
    VERIFY( density[2] == (preserved ? wt[2] : InputType(1)) );
  };

  Distribution from_iter(x.begin(), x.end(), wt);
  validate(from_iter);

  auto wf = [&wt](DistType v)
  { return wt[int(v / 0.5)]; };

  Distribution from_init_list(x, wf);
  validate(from_init_list);

#if _GLIBCXX_USE_NEW_PIECEWISE_DISTRIBUTIONS
  // PR125548 leads to incorrect densities being used
  Distribution from_count(2, InputType(0), InputType(1), wf);
  validate(from_count);
#endif
}

template<typename RealType, typename URBG = std::mt19937>
void
test_engine_calls()
{
  constexpr std::size_t bits = __builtin_popcountg(URBG::max() - URBG::min());
  constexpr std::size_t calls
#if _GLIBCXX_USE_NEW_PIECEWISE_DISTRIBUTIONS
    = (std::numeric_limits<RealType>::digits + bits - 1) / bits;
#else
    = (std::numeric_limits<double>::digits + bits - 1) / bits;
#endif

  struct wrapper : URBG
  {
    typename URBG::result_type operator()()
    {
      ++num_calls;
      return URBG::operator()();
    }
    std::size_t num_calls = 0;
  };

  wrapper eng;
  std::piecewise_linear_distribution<RealType> u;
  u(eng);
  VERIFY(eng.num_calls == calls);
}

int main()
{
  using namespace __gnu_test;
  test_exact<float>();
  test_exact<double>();
  test_exact<long double>();

  test_precision_depended<float>();
  test_precision_depended<double>();
  test_precision_depended<long double>();

  test_precision_depended<double, float>();
  test_precision_depended<long double, float>();
  test_precision_depended<long double, double>();

  test_engine_calls<float>();
  test_engine_calls<double>();
  test_engine_calls<long double>();
  return 0;
}
