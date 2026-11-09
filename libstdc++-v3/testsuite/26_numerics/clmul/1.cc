// { dg-do run { target c++29 } }

#include <numeric>

#ifndef __cpp_lib_clmul
# error "Feature-test macro for clmul missing in <numeric>"
#elif __cpp_lib_clmul != 202610L
# error "Feature-test macro for clmul has wrong value in <numeric>"
#endif

#include <testsuite_hooks.h>

static_assert(std::wide_result<unsigned>(42U, 43U).low() == 43);
static_assert(std::wide_result<unsigned>(42U, 43U).high() == 42);
static_assert(std::wide_result<unsigned>(42U, 43U)
	      == std::wide_result<unsigned>(42U, 43U));
static_assert(std::wide_result<unsigned>(42U, 43U)
	      != std::wide_result<unsigned>(42U, 44U));
static_assert(std::wide_result<unsigned>(42U, 43U)
	      != std::wide_result<unsigned>(43U, 43U));
static_assert(std::wide_result<unsigned>(42U, 45U)
	      < std::wide_result<unsigned>(43U, 43U));
static_assert(std::wide_result<unsigned>(42U, 43U)
	      < std::wide_result<unsigned>(42U, 44U));
static_assert(std::wide_result<unsigned>(42U, 43U)
	      > std::wide_result<unsigned>(41U, 44U));
static_assert(std::wide_result<unsigned>(42U, 43U)
	      > std::wide_result<unsigned>(42U, 42U));
static_assert(std::is_same<decltype(std::widening_clmul((unsigned char) 0,
							(unsigned char) 0)),
			   std::wide_result<unsigned char>>::value);
static_assert(std::is_same<decltype(std::widening_clmul((unsigned short) 0,
							(unsigned short) 0)),
			   std::wide_result<unsigned short>>::value);
static_assert(std::is_same<decltype(std::widening_clmul(0U, 0U)),
			   std::wide_result<unsigned int>>::value);
static_assert(std::is_same<decltype(std::widening_clmul(0UL, 0UL)),
			   std::wide_result<unsigned long>>::value);
static_assert(std::is_same<decltype(std::widening_clmul(0ULL, 0ULL)),
			   std::wide_result<unsigned long long>>::value);
#if __CHAR_BIT__ == 8
static_assert(std::clmul((unsigned char) 0x7e,
			 (unsigned char) 0x9c) == 0xe8);
static_assert(std::widening_clmul((unsigned char) 0x7e,
				  (unsigned char) 0x9c).low() == 0xe8);
static_assert(std::widening_clmul((unsigned char) 0x7e,
				  (unsigned char) 0x9c).high() == 0x3a);
#if __SIZEOF_SHORT__ == 2
static_assert(std::clmul((unsigned short) 0xc8aa,
			 (unsigned short) 0x7616) == 0xa55c);
static_assert(std::widening_clmul((unsigned short) 0xc8aa,
				  (unsigned short) 0x7616)
	      == std::wide_result<unsigned short>(0x2508, 0xa55c));
#endif
#if __SIZEOF_INT__ == 4
static_assert(std::clmul(0x9f330680U, 0x4e157eadU) == 0xb23fb880U);
static_assert(std::widening_clmul(0x9f330680U, 0x4e157eadU)
	      == std::wide_result<unsigned>(0x207cd974U, 0xb23fb880U));
static_assert(std::clmul(0x11111111U, 0x11111111U) == 0x1010101U);
static_assert(std::widening_clmul(0x11111111U, 0x11111111U)
	      == std::wide_result<unsigned>(0x1010101U, 0x1010101U));
static_assert(std::clmul(~0U, ~0U) == 0x55555555U);
static_assert(std::widening_clmul(~0U, ~0U)
	      == std::wide_result<unsigned>(0x55555555U, 0x55555555U));
#endif
#if __SIZEOF_LONG__ == 4
static_assert(std::clmul(0xcd63e2c1UL, 0xab176a37UL) == 0x2f053277UL);
static_assert(std::widening_clmul(0xcd63e2c1UL, 0xab176a37UL)
	      == std::wide_result<unsigned long>(0x782faf0eUL, 0x2f053277UL));
#elif __SIZEOF_LONG__ == 8
static_assert(std::clmul(0x43e13a77bec666afUL, 0x39c165e2ed3f98f7UL)
	      == 0xe8465657c87e7a1dUL);
static_assert(std::widening_clmul(0x43e13a77bec666afUL, 0x39c165e2ed3f98f7UL)
	      == std::wide_result<unsigned long>(0xe2f8ec31f25371bUL,
						 0xe8465657c87e7a1dUL));
#endif
#if __SIZEOF_LONG_LONG__ == 8
static_assert(std::clmul(0xe38ebed3ef3cc8deULL, 0x75224424100f8096ULL)
	      == 0xfaf0ddab7a75024ULL);
static_assert(std::widening_clmul(0xe38ebed3ef3cc8deULL, 0x75224424100f8096ULL)
	      == std::wide_result<unsigned long long>(0x29daaa7ee7cd2860ULL,
						      0xfaf0ddab7a75024ULL));
static_assert(std::clmul(0x11111111ULL, 0x11111111ULL)
	      == 0x101010101010101ULL);
static_assert(std::widening_clmul(0x11111111ULL, 0x11111111ULL)
	      == std::wide_result<unsigned long long>(0ULL,
						      0x101010101010101ULL));
static_assert(std::clmul(0x1111111111111111ULL, 0x1111111111111111ULL)
	      == 0x101010101010101ULL);
static_assert(std::widening_clmul(0x1111111111111111ULL, 0x1111111111111111ULL)
	      == std::wide_result<unsigned long long>(0x101010101010101ULL,
						      0x101010101010101ULL));
static_assert(std::clmul(~0ULL, ~0ULL)
	      == 0x5555555555555555ULL);
static_assert(std::widening_clmul(~0ULL, ~0ULL)
	      == std::wide_result<unsigned long long>(0x5555555555555555ULL,
						      0x5555555555555555ULL));
#endif
#if __SIZEOF_INT128__ == 16
static_assert(std::clmul((((unsigned __int128) 0x983a442a106b8a83ULL) << 64)
			 | 0x1866ccfa2e2b382cULL,
			 (((unsigned __int128) 0xe9abf4d856d535c3ULL) << 64)
			 | 0x58708f117a52c14cULL)
	      == ((((unsigned __int128) 0xd3ae65b0bac566b2ULL) << 64)
		  | 0xdd6b33f9bb8606d0ULL));
static_assert(std::widening_clmul((((unsigned __int128) 0x983a442a106b8a83ULL)
				   << 64) | 0x1866ccfa2e2b382cULL,
				  (((unsigned __int128) 0xe9abf4d856d535c3ULL)
				   << 64) | 0x58708f117a52c14cULL)
	      == std::wide_result<unsigned __int128>((((unsigned __int128)
						       0x7d17129902648920ULL)
						      << 64)
						     | 0xfdb34e3b7c058047ULL,
						     (((unsigned __int128)
						       0xd3ae65b0bac566b2ULL)
						      << 64)
						     | 0xdd6b33f9bb8606d0ULL));
#endif
#endif

template<typename T>
[[gnu::noipa]] std::wide_result<T>
myclmul(T x, T y)
{
  auto r = std::widening_clmul(x, y);
  VERIFY( std::clmul(x, y) == r.low() );
  return r;
}

template<typename T>
using W = std::wide_result<T>;

void
test01()
{
#if __CHAR_BIT__ == 8
  VERIFY( myclmul((unsigned char) 0x7e, (unsigned char) 0x9c)
	  == W<unsigned char>(0x3a, 0xe8) );
#if __SIZEOF_SHORT__ == 2
  VERIFY( myclmul((unsigned short) 0xc8aa, (unsigned short) 0x7616)
	  == W<unsigned short>(0x2508, 0xa55c) );
#endif
#if __SIZEOF_INT__ == 4
  VERIFY( myclmul(0x9f330680U, 0x4e157eadU)
	  == W<unsigned>(0x207cd974U, 0xb23fb880U) );
  VERIFY( myclmul(0x11111111U, 0x11111111U)
	  == W<unsigned>(0x1010101U, 0x1010101U) );
  VERIFY( myclmul(~0U, ~0U)
	  == W<unsigned>(0x55555555U, 0x55555555U) );
#endif
#if __SIZEOF_LONG__ == 4
  VERIFY( myclmul(0xcd63e2c1UL, 0xab176a37UL)
	  == W<unsigned long>(0x782faf0eUL, 0x2f053277UL) );
#elif __SIZEOF_LONG__ == 8
  VERIFY( myclmul(0x43e13a77bec666afUL, 0x39c165e2ed3f98f7UL)
	  == W<unsigned long>(0xe2f8ec31f25371bUL, 0xe8465657c87e7a1dUL) );
#endif
#if __SIZEOF_LONG_LONG__ == 8
  VERIFY( myclmul(0xe38ebed3ef3cc8deULL, 0x75224424100f8096ULL)
	  == W<unsigned long long>(0x29daaa7ee7cd2860ULL,
				   0xfaf0ddab7a75024ULL) );
  VERIFY( myclmul(0x11111111ULL, 0x11111111ULL)
	  == W<unsigned long long>(0ULL, 0x101010101010101ULL) );
  VERIFY( myclmul(0x1111111111111111ULL, 0x1111111111111111ULL)
	  == W<unsigned long long>(0x101010101010101ULL,
				   0x101010101010101ULL) );
  VERIFY( myclmul(~0ULL, ~0ULL)
	  == W<unsigned long long>(0x5555555555555555ULL,
				   0x5555555555555555ULL) );
#endif
#if __SIZEOF_INT128__ == 16
  VERIFY( myclmul((((unsigned __int128) 0x983a442a106b8a83ULL) << 64)
		  | 0x1866ccfa2e2b382cULL,
		  (((unsigned __int128) 0xe9abf4d856d535c3ULL) << 64)
		  | 0x58708f117a52c14cULL)
	  == W<unsigned __int128>((((unsigned __int128) 0x7d17129902648920ULL)
				   << 64)
				  | 0xfdb34e3b7c058047ULL,
				  (((unsigned __int128) 0xd3ae65b0bac566b2ULL)
				   << 64)
				  | 0xdd6b33f9bb8606d0ULL) );
#endif
#endif
}

template<typename T>
void
test02()
{
  VERIFY( myclmul(T(), T()) == W<T>(T(), T()) );
}

int
main()
{
  test01();
  test02<unsigned char>();
  test02<unsigned short>();
  test02<unsigned int>();
  test02<unsigned long>();
  test02<unsigned long long>();
#ifdef __SIZEOF_INT128__
  test02<unsigned __int128>();
#endif
}
