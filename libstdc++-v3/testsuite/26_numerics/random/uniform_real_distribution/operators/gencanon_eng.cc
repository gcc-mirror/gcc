// { dg-do compile { target { c++11 } } }

#include <random>

template<typename _Real, typename _URBG>
void
test_engine()
{
  _URBG __engine;
  (void)std::generate_canonical<_Real, size_t(-1)>(__engine);
}

template<std::uint64_t Max, typename Under = std::mt19937_64>
struct trimmed_engine
{
  using result_type = std::uint64_t;

  static constexpr
  result_type min()
  { return result_type(0); }

  static constexpr
  result_type max()
  { return result_type(Max); }

  trimmed_engine() : dist(min(), max())
  {}

  result_type operator()()
  { return dist(under); }

private:
  Under under;
  std::uniform_int_distribution<result_type> dist;
};

template<typename Real, size_t Bits>
void
test_bits()
{
  trimmed_engine<(std::uint64_t(1) << Bits) - 1> pow2_engine;
  (void)std::generate_canonical<Real, -1u>(pow2_engine);
  trimmed_engine<(std::uint64_t(1) << Bits) - 2> high_non_pow2_engine;
  (void)std::generate_canonical<Real, -1u>(high_non_pow2_engine);
  trimmed_engine<(std::uint64_t(1) << (Bits - 1))> low_non_pow2_engine;
  (void)std::generate_canonical<Real, -1u>(low_non_pow2_engine);
}

template<typename _Real>
void
test_all_engines()
{
  test_engine<_Real, std::default_random_engine>();

  test_engine<_Real, std::minstd_rand0>();
  test_engine<_Real, std::minstd_rand>();
  test_engine<_Real, std::mt19937>();
  test_engine<_Real, std::mt19937_64>();
  test_engine<_Real, std::ranlux24_base>();
  test_engine<_Real, std::ranlux48_base>();
  test_engine<_Real, std::ranlux24>();
  test_engine<_Real, std::ranlux48>();
  test_engine<_Real, std::knuth_b>();
#if __cplusplus > 202302L
  test_engine<_Real, std::philox4x32>();
  test_engine<_Real, std::philox4x64>();
#endif

  // For 128bit floating points, generator emitting a range, which size is
  // not power of two, but of width of B bits, such that for any N:
  // N * (B-1) < 113 (bits in ieee128)
  // (N+1) * B > 128
  // use >128bits patch, as they would otherwise require integer with more
  // than 128 bits.
  // N == 3: B in [43, 57]
  test_bits<_Real, 43>();
  test_bits<_Real, 57>();

  // N == 4: B in [33, 38]
  test_bits<_Real, 33>();
  test_bits<_Real, 38>();

  // N == 5: B in [26, 29]
  test_bits<_Real, 26>();
  test_bits<_Real, 29>();

  // N == 6: B in [22, 23]
  test_bits<_Real, 22>();
  test_bits<_Real, 23>();

  // N == 7, B == 19
  test_bits<_Real, 19>();

  // N == 8, B == 17
  test_bits<_Real, 17>();

  // N == 9, B == 15
  test_bits<_Real, 15>();

  // N >= 10 and B < 13
  test_bits<_Real, 13>();
}

int main()
{
  test_all_engines<float>();
  test_all_engines<double>();
  test_all_engines<long double>();
#ifndef _GLIBCXX_GENERATE_CANONICAL_STRICT
#  ifdef __SIZEOF_FLOAT128__
  test_all_engines<__float128>();
#  endif
#endif
}
