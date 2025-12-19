// { dg-do compile { target { c++11 } } }

#include <random>

template<typename _Real, typename _URBG>
void
test_engine()
{
  _URBG __engine;
  (void)std::generate_canonical<_Real, size_t(-1)>(__engine);
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
