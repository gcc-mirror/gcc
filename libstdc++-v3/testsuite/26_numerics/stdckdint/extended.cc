// { dg-do run { target c++26 } }

#include <stdckdint.h>
#include <limits>
#include <testsuite_hooks.h>

template<typename T>
void
test()
{
  using S = std::make_signed_t<T>;
  using U = std::make_unsigned_t<T>;

  constexpr S smax = std::numeric_limits<S>::max();
  constexpr S smin = std::numeric_limits<S>::min();
  constexpr U umax = std::numeric_limits<U>::max();
  S sout{};
  U uout{};

  VERIFY( ckd_add(&sout, smax, (S)1) );
  VERIFY( ! ckd_add(&uout, smax, (U)1) && uout == ((U)smax + (U)1) );
  VERIFY( ! ckd_add(&sout, smin, (S)99) && sout == (smin + 99) );
  VERIFY( ckd_add(&uout, smin, (S)99) );
  VERIFY( ckd_add(&sout, smin, (S)-2) );
  VERIFY( ckd_add(&uout, umax, (U)3) );
  VERIFY( ! ckd_add(&sout, (U)9, (U)3) && sout == 12 );

  VERIFY( ckd_sub(&sout, smax, (S)-1) );
  VERIFY( ! ckd_sub(&uout, smax, (S)-1) && uout == ((U)smax + (U)1) );
  VERIFY( ckd_sub(&sout, smin, (S)2) );
  VERIFY( ! ckd_sub(&sout, smin, (S)-2) && sout == (smin + 2) );
  VERIFY( ! ckd_sub(&sout, (U)0, (U)3) && sout == -3 );
  VERIFY( ckd_sub(&uout, (U)0, (U)3) );

  VERIFY( ! ckd_mul(&sout, (S)(smax >> 2), (S)3) && sout == (smax/4*3) );
  VERIFY( ckd_mul(&sout, (S)(smax >> 1), (S)3) );
  VERIFY( ! ckd_mul(&uout, (S)(smax >> 1), (S)3) );
  VERIFY( ckd_mul(&sout, (S)(smin >> 1), (S)5) );
  VERIFY( ! ckd_mul(&uout, (U)(umax >> 2), (U)3) );
  VERIFY( ckd_mul(&sout, (U)(umax >> 2), (U)3) );
  VERIFY( ckd_mul(&uout, (U)(umax >> 1), (U)7) );
}

int main()
{
#ifdef __SIZEOF_INT128__
  test<__int128>();
#endif

#ifdef __GLIBCXX_TYPE_INT_N_0
  test<__GLIBCXX_TYPE_INT_N_0>();
#endif

#ifdef __GLIBCXX_TYPE_INT_N_1
  test<__GLIBCXX_TYPE_INT_N_1>();
#endif

#ifdef __GLIBCXX_TYPE_INT_N_2
  test<__GLIBCXX_TYPE_INT_N_2>();
#endif

#ifdef __GLIBCXX_TYPE_INT_N_3
  test<__GLIBCXX_TYPE_INT_N_3>();
#endif
}
