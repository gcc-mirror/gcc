// { dg-do compile { target c++29 } }

#include <bit>
#include <limits>

template<typename Int, typename SInt>
constexpr bool
test_negative_shifts()
{
  constexpr unsigned digits = std::numeric_limits<Int>::digits
			      + std::is_signed_v<Int>;

  Int xarr[] = { (Int)-1, 0, 1, 3, 6, 7, 0x10, 0x11, 0x22, 0x44, (Int)0x80,
		 (Int)-55, (Int)-927 };
  SInt sarr[] = { 1, 4, 5, digits - 1, digits, (SInt)-12 };
  for (Int x : xarr)
    for (SInt s : sarr)
      if (std::shl(x, -s) != std::shr(x, s))
	return false;
  return true;
}

template<typename Int, typename SInt>
constexpr auto
test(Int x, SInt y)
-> decltype(std::shl(x, y))
{
  static_assert( noexcept(std::shl(x, 0)) );

  constexpr unsigned digits = std::numeric_limits<Int>::digits
			      + std::is_signed_v<Int>;

  static_assert( std::shl((Int)0, (SInt)0) == 0 );
  static_assert( std::shl((Int)0, (SInt)1) == 0 );
  static_assert( std::shl((Int)0, (SInt)4) == 0 );
  static_assert( std::shl((Int)0, (SInt)8) == 0 );
  static_assert( std::shl((Int)0, (SInt)-4) == 0 );
  static_assert( std::shl((Int)-1, (SInt)0) == (Int)((Int)-1 << 0) );
  static_assert( std::shl((Int)-1, (SInt)1) == (Int)((Int)-1 << 1) );
  static_assert( std::shl((Int)-1, (SInt)4) == (Int)((Int)-1 << 4) );
  static_assert( std::shl((Int)-1, (SInt)8) == (Int)((Int)-1 << 8) );
  static_assert( std::shl((Int)-1, (SInt)-4)
		 == ((SInt)-4 < 0 ? (Int)-1 >> (SInt)4
		     : (SInt)-4 >= digits ? 0
		     : (Int)((Int)-1 << ((SInt)-4 < 0 ? 0 : (SInt)-4))) );

  static_assert( std::shl((Int)1, (SInt)0) == (Int)1 << 0 );
  static_assert( std::shl((Int)1, (SInt)1) == (Int)1 << 1 );
  static_assert( std::shl((Int)1, (SInt)4) == (Int)1 << 4 );
  static_assert( std::shl((Int)1, (SInt)digits) == (Int)0 );
  static_assert( std::shl((Int)7, (SInt)digits) == (Int)0 );
  static_assert( std::shl((Int)6, (SInt)digits - 1) == (Int)0 );
  static_assert( std::shl((Int)3, (SInt)6) == (Int)((Int)3 << 6) );

  static_assert( std::shl((Int)0b0110'1100, (SInt)1) == (Int)0b1101'1000 );
  static_assert( std::shl((Int)0b0110'1100, (SInt)digits - 1) == 0 );

  static_assert( std::shl((Int)0x01, (SInt)0 ) == 0x01 );
  static_assert( std::shl((Int)0x10, (SInt)0 ) == 0x10 );
  static_assert( std::shl((Int)0x10, (SInt)1 ) == 0x20 );
  static_assert( std::shl((Int)0x10, (SInt)2 ) == 0x40 );
  static_assert( std::shl((Int)0x10, (SInt)3 ) == (Int)0x80 );
  static_assert( std::shl((Int)0x11, (SInt)1 ) == 0x22 );
  static_assert( std::shl((Int)0x11, (SInt)2 ) == 0x44 );
  static_assert( std::shl((Int)0x11, (SInt)-2 )
		 == ((SInt)-2 < 0 ? 4
		     : (SInt)-2 >= digits ? 0
		     : (Int)0x11 << ((SInt)-2 < 0 ? 0 : (SInt)-2)) );

  if constexpr (std::numeric_limits<Int>::digits > 8)
    {
      static_assert( std::shl((Int)0b0011'0111, (SInt)3) == 0b1'1011'1000 );
      static_assert( std::shl((Int)0b1010'0101, (SInt)4) == 0b1010'0101'0000 );
    }

  if constexpr (std::is_signed_v<SInt>)
    static_assert( test_negative_shifts<Int, SInt>() );

  return true;
}

static_assert( test( (unsigned char)0, (unsigned short)0 ) );
static_assert( test( (unsigned short)0, (unsigned int)0 ) );
static_assert( test( (unsigned int)0, (unsigned long)0 ) );
static_assert( test( (unsigned long)0, (unsigned long long)0 ) );
static_assert( test( (unsigned long long)0, (unsigned char)0 ) );
static_assert( test( (signed char)0, (unsigned int)0 ) );
static_assert( test( (signed short)0, (unsigned long)0 ) );
static_assert( test( (signed int)0, (unsigned long long)0 ) );
static_assert( test( (signed long)0, (unsigned char)0 ) );
static_assert( test( (signed long long)0, (unsigned short)0 ) );
static_assert( test( (unsigned char)0, (signed long long)0 ) );
static_assert( test( (unsigned short)0, (signed char)0 ) );
static_assert( test( (unsigned int)0, (signed short)0 ) );
static_assert( test( (unsigned long)0, (signed int)0 ) );
static_assert( test( (unsigned long long)0, (signed long)0 ) );
static_assert( test( (signed char)0, (signed char)0 ) );
static_assert( test( (signed short)0, (signed short)0 ) );
static_assert( test( (signed int)0, (signed int)0 ) );
static_assert( test( (signed long)0, (signed long)0 ) );
static_assert( test( (signed long long)0, (signed long long)0 ) );

struct X { constexpr bool did_not_match() { return true; } };
constexpr X test(...) { return X{}; }
static_assert( test( (bool)0, 0 ).did_not_match() );
static_assert( test( (char)0, 0 ).did_not_match() );
static_assert( test( (char16_t)0, 0 ).did_not_match() );
static_assert( test( (float)0, 0 ).did_not_match() );
static_assert( test( (void*)0, 0 ).did_not_match() );
static_assert( test( X{}, 0 ).did_not_match() );
static_assert( test( 0U, (bool)0 ).did_not_match() );
static_assert( test( 0U, (char)0 ).did_not_match() );
static_assert( test( 0U, (char16_t)0 ).did_not_match() );
static_assert( test( 0, (float)0 ).did_not_match() );
static_assert( test( 0, (void*)0 ).did_not_match() );
static_assert( test( 0, X{} ).did_not_match() );
enum E : unsigned { e };
static_assert( test( e, 0 ).did_not_match() );
static_assert( test( 0U, e ).did_not_match() );

#if !defined(__STRICT_ANSI__) && defined __SIZEOF_INT128__
static_assert( test( (unsigned __int128)0, (__int128)0 ) );
static_assert( test( (__int128)0, (unsigned __int128)0 ) );
static_assert( test( (unsigned __int128)0, 0U ) );
static_assert( test( (__int128)0, 0 ) );
#endif
#if defined(__GLIBCXX_TYPE_INT_N_0)
static_assert( test( (unsigned __GLIBCXX_TYPE_INT_N_0)0, (__GLIBCXX_TYPE_INT_N_0)0 ) );
static_assert( test( (__GLIBCXX_TYPE_INT_N_0)0, (unsigned __GLIBCXX_TYPE_INT_N_0)0 ) );
static_assert( test( (unsigned __GLIBCXX_TYPE_INT_N_0)0, 0U ) );
static_assert( test( (__GLIBCXX_TYPE_INT_N_0)0, 0 ) );
#endif
#if defined(__GLIBCXX_TYPE_INT_N_1)
static_assert( test( (unsigned __GLIBCXX_TYPE_INT_N_1)0, (__GLIBCXX_TYPE_INT_N_1)0 ) );
static_assert( test( (__GLIBCXX_TYPE_INT_N_1)0, (unsigned __GLIBCXX_TYPE_INT_N_1)0 ) );
static_assert( test( (unsigned __GLIBCXX_TYPE_INT_N_1)0, 0U ) );
static_assert( test( (__GLIBCXX_TYPE_INT_N_1)0, 0 ) );
#endif
#if defined(__GLIBCXX_TYPE_INT_N_2)
static_assert( test( (unsigned __GLIBCXX_TYPE_INT_N_2)0, (__GLIBCXX_TYPE_INT_N_2)0 ) );
static_assert( test( (__GLIBCXX_TYPE_INT_N_2)0, (unsigned __GLIBCXX_TYPE_INT_N_2)0 ) );
static_assert( test( (unsigned __GLIBCXX_TYPE_INT_N_2)0, 0U ) );
static_assert( test( (__GLIBCXX_TYPE_INT_N_2)0, 0 ) );
#endif
#if defined(__GLIBCXX_TYPE_INT_N_3)
static_assert( test( (unsigned __GLIBCXX_TYPE_INT_N_3)0, (__GLIBCXX_TYPE_INT_N_3)0 ) );
static_assert( test( (__GLIBCXX_TYPE_INT_N_3)0, (unsigned __GLIBCXX_TYPE_INT_N_3)0 ) );
static_assert( test( (unsigned __GLIBCXX_TYPE_INT_N_3)0, 0U ) );
static_assert( test( (__GLIBCXX_TYPE_INT_N_3)0, 0 ) );
#endif

static_assert( std::shl(-1, 0x7fffffff00000001LL) == 0 );
static_assert( std::shl(2, 0x7fffffff00000001LL) == 0 );
static_assert( std::shl(-1, -0x7fffffff00000001LL) == -1 );
static_assert( std::shl(2, -0x7fffffff00000001LL) == 0 );

#include <cstddef>
static_assert( test( (std::byte)0, 0 ).did_not_match() );
static_assert( test( 0, (std::byte)0 ).did_not_match() );
