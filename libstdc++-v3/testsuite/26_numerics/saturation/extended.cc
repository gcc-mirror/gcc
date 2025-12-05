// { dg-do compile { target c++26 } }

#include <numeric>
#include <limits>

template<typename T>
constexpr bool
test()
{
  using S = std::make_signed_t<T>;
  using U = std::make_unsigned_t<T>;

  constexpr S smax = std::numeric_limits<S>::max();
  constexpr S smin = std::numeric_limits<S>::min();
  constexpr U umax = std::numeric_limits<U>::max();

  static_assert( std::add_sat(smax, (S)1) == smax );
  static_assert( std::add_sat(smin, (S)-2) == smin );
  static_assert( std::add_sat(umax, (U)3) == umax );

  static_assert( std::sub_sat(smax, (S)-1) == smax );
  static_assert( std::sub_sat(smin, (S)2) == smin );
  static_assert( std::sub_sat((U)0, (U)3) == (U)0 );

  static_assert( std::mul_sat((S)(smax >> 1), (S)3) == smax );
  static_assert( std::mul_sat((S)(smin >> 1), (S)5) == smin );
  static_assert( std::mul_sat((U)(umax >> 1), (U)7) == umax );

  static_assert( std::div_sat(smax, (S)2) == (smax >> 1) );
  static_assert( std::div_sat(smin, (S)4) == (smin >> 2) );
  static_assert( std::div_sat(smin, (S)-1) == smax );
  static_assert( std::div_sat(umax, (U)8) == (umax >> 3) );

  return true;
}

#ifdef __SIZEOF_INT128__
static_assert(test<__int128>());
#endif

#ifdef __GLIBCXX_TYPE_INT_N_0
static_assert(test<__GLIBCXX_TYPE_INT_N_0>());
#endif

#ifdef __GLIBCXX_TYPE_INT_N_1
static_assert(test<__GLIBCXX_TYPE_INT_N_1>());
#endif

#ifdef __GLIBCXX_TYPE_INT_N_2
static_assert(test<__GLIBCXX_TYPE_INT_N_2>());
#endif

#ifdef __GLIBCXX_TYPE_INT_N_3
static_assert(test<__GLIBCXX_TYPE_INT_N_3>());
#endif
