// { dg-do compile { target c++20 } }

#include <utility>

template<typename T>
constexpr bool
test()
{
  using S = std::make_signed_t<T>;
  using U = std::make_unsigned_t<T>;

  static_assert( std::cmp_less((S)-1, (U)1));
  static_assert( ! std::cmp_less((S)20, (U)10));
  static_assert( ! std::cmp_less((U)20, (S)10));

  static_assert( std::cmp_greater((S)100, (U)1) );
  static_assert( std::cmp_greater((U)100, (S)1) );
  static_assert( ! std::cmp_greater((S)-100, (U)1) );

  static_assert( std::cmp_less_equal((S)-1, (U)1));
  static_assert( std::cmp_less_equal((U)10, (S)10));
  static_assert( ! std::cmp_less_equal((U)-100, (S)-100));

  static_assert( std::cmp_greater_equal((S)200, (U)2) );
  static_assert( std::cmp_greater_equal((U)2000, (S)2000) );
  static_assert( ! std::cmp_greater_equal((S)-100, (U)100) );

  static_assert( std::cmp_equal((S)1, (U)1) );
  static_assert( ! std::cmp_equal((S)-2, (U)-2) );

  static_assert( std::cmp_not_equal((S)-1, (U)-1) );
  static_assert( ! std::cmp_not_equal((S)100, (U)100) );

  static_assert( std::in_range<S>((U)5) );
  static_assert( std::in_range<U>((S)5) );
  static_assert( ! std::in_range<S>((U)-5) );
  static_assert( ! std::in_range<U>((S)-5) );

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
