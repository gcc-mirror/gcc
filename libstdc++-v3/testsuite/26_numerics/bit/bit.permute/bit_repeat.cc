// { dg-do compile { target c++29 } }

#include <bit>
#include <limits>

// Detect whether std::bit_repeat(T(0), N) is a constant expression.
template<typename T, auto N, typename = void>
  struct bit_repeat_valid
  : std::false_type { };

template<typename T, auto N>
  struct bit_repeat_valid<T, N, std::void_t<char[(std::bit_repeat(T(0), N) == T(0))]>>
  : std::true_type { };

template<typename UInt>
constexpr auto
test(UInt x)
-> decltype(std::bit_repeat(x, 1))
{
  constexpr unsigned digits = std::numeric_limits<UInt>::digits;
  static_assert( std::bit_repeat(UInt(0), 1) == UInt(0) );
  static_assert( std::bit_repeat(UInt(0), digits) == UInt(0) );
  static_assert( std::bit_repeat(UInt(0), digits * 6) == UInt(0) );
  static_assert( std::bit_repeat(UInt(~UInt(0)), 1) == UInt(~UInt(0)) );
  static_assert( std::bit_repeat(UInt(~UInt(0)), digits) == UInt(~UInt(0)) );
  static_assert( std::bit_repeat(UInt(~UInt(0)), digits * 6) == UInt(~UInt(0)) );
  static_assert( std::bit_repeat(UInt(1), 1) == UInt(~UInt(0)) );
  static_assert( std::bit_repeat(UInt(~UInt(1)), 1) == UInt(0) );
  static_assert( std::bit_repeat(std::bit_repeat(UInt(0x0123456789abcdefull), 3), 3)
		 == std::bit_repeat(UInt(0x0123456789abcdefull), 3) );
  if constexpr (std::numeric_limits<UInt>::digits >= 8)
    {
      static_assert( (std::bit_repeat<UInt>(0x34, 3) & UInt(0xff)) == 0x24u );
      static_assert( (std::bit_repeat<UInt>(0x71, 2) & UInt(0xff)) == 0x55u );
      static_assert( (std::bit_repeat<UInt>(0xf2, 2) & UInt(0xff)) == 0xaau );
      static_assert( (std::bit_repeat<UInt>(0xb3, 5) & UInt(0xff)) == 0x73u );
      static_assert( (std::bit_repeat<UInt>(0xb3, 8) & UInt(0xff)) == 0xb3u );
    }
  if constexpr (std::numeric_limits<UInt>::digits >= 16)
    {
      static_assert( (std::bit_repeat<UInt>(0x1734, 3) & UInt(0xffff)) == 0x4924u );
      static_assert( (std::bit_repeat<UInt>(0x2071, 2) & UInt(0xffff)) == 0x5555u );
      static_assert( (std::bit_repeat<UInt>(0x52f2, 2) & UInt(0xffff)) == 0xaaaau );
      static_assert( (std::bit_repeat<UInt>(0x07b3, 5) & UInt(0xffff)) == 0xce73u );
      static_assert( (std::bit_repeat<UInt>(0x21b3, 8) & UInt(0xffff)) == 0xb3b3u );
      static_assert( (std::bit_repeat<UInt>(0xa2b3u, 16) & UInt(0xffff)) == 0xa2b3u );
    }
  if constexpr (std::numeric_limits<UInt>::digits >= 32)
    {
      static_assert( (std::bit_repeat<UInt>(0x12345634u, 3) & UInt(0xffffffffu)) == 0x24924924u );
      static_assert( (std::bit_repeat<UInt>(0x23456771u, 2) & UInt(0xffffffffu)) == 0x55555555u );
      static_assert( (std::bit_repeat<UInt>(0x345678f2u, 2) & UInt(0xffffffffu)) == 0xaaaaaaaau );
      static_assert( (std::bit_repeat<UInt>(0x456789b3u, 5) & UInt(0xffffffffu)) == 0xe739ce73u );
      static_assert( (std::bit_repeat<UInt>(0x56789ab3u, 8) & UInt(0xffffffffu)) == 0xb3b3b3b3u );
      static_assert( (std::bit_repeat<UInt>(0x6789a2b3u, 16) & UInt(0xffffffffu)) == 0xa2b3a2b3u );
      static_assert( (std::bit_repeat<UInt>(0x2a4dda0eu, 32) & UInt(0xffffffffu)) == 0x2a4dda0eu );
    }
  if constexpr (std::numeric_limits<UInt>::digits >= 64)
    {
      static_assert( (std::bit_repeat<UInt>(0x1122334455667734ull, 3) & UInt(0xffffffffffffffffull)) == 0x4924924924924924ull );
      static_assert( (std::bit_repeat<UInt>(0x2233445566778871ull, 2) & UInt(0xffffffffffffffffull)) == 0x5555555555555555ull );
      static_assert( (std::bit_repeat<UInt>(0x33445566778899f2ull, 2) & UInt(0xffffffffffffffffull)) == 0xaaaaaaaaaaaaaaaaull );
      static_assert( (std::bit_repeat<UInt>(0x445566778899aab3ull, 5) & UInt(0xffffffffffffffffull)) == 0x39ce739ce739ce73ull );
      static_assert( (std::bit_repeat<UInt>(0x5566778899aabbb3ull, 8) & UInt(0xffffffffffffffffull)) == 0xb3b3b3b3b3b3b3b3ull );
      static_assert( (std::bit_repeat<UInt>(0x66778899aabba2b3ull, 16) & UInt(0xffffffffffffffffull)) == 0xa2b3a2b3a2b3a2b3ull );
      static_assert( (std::bit_repeat<UInt>(0x778899aa2a4dda0eull, 32) & UInt(0xffffffffffffffffull)) == 0x2a4dda0e2a4dda0eull );
    }
  if constexpr (std::numeric_limits<UInt>::digits == 128)
    {
      static_assert( std::bit_repeat<UInt>(1, 2)
		     == (UInt(0x5555555555555555ull) << 64) | 0x5555555555555555ull );
      static_assert( std::bit_repeat<UInt>(2, 2)
		     == (UInt(0xaaaaaaaaaaaaaaaaull) << 64) | 0xaaaaaaaaaaaaaaaaull );
    }
  static_assert( bit_repeat_valid<UInt, 1>() );
  static_assert( bit_repeat_valid<UInt, 17>() );
  static_assert( bit_repeat_valid<UInt, digits>() );
  static_assert( bit_repeat_valid<UInt, digits * 8>() );
  static_assert( !bit_repeat_valid<UInt, 0>() );
  static_assert( !bit_repeat_valid<UInt, -1>() );
  static_assert( !bit_repeat_valid<UInt, -5>() );

  return x;
}

static_assert( test( (unsigned char)123 ) );
static_assert( test( (unsigned short)12345 ) );
static_assert( test( (unsigned int)1234567890 ) );
static_assert( test( 123456789UL ) );
static_assert( test( ~123456789ULL ) );

// std::bit_repeat(T, N) shall not participate in overload resolution
// unless T is an unsigned integer type.
struct X { constexpr bool did_not_match() { return true; } };
constexpr X test(...) { return X{}; }
static_assert( test( (bool)0 ).did_not_match() );
static_assert( test( (char)0 ).did_not_match() );
static_assert( test( (int)0 ).did_not_match() );
static_assert( test( (char16_t)0 ).did_not_match() );
static_assert( test( (float)0 ).did_not_match() );
static_assert( test( (void*)0 ).did_not_match() );
static_assert( test( X{} ).did_not_match() );
enum E : unsigned { e };
static_assert( test( e ).did_not_match() );
  
#if !defined(__STRICT_ANSI__) && defined __SIZEOF_INT128__
static_assert( test( (unsigned __int128)3 ) );
static_assert( test( (__int128)0 ).did_not_match() );
#endif
#if defined(__GLIBCXX_TYPE_INT_N_0)
static_assert( test( (unsigned __GLIBCXX_TYPE_INT_N_0)1 ) );
static_assert( test( (__GLIBCXX_TYPE_INT_N_0)0 ).did_not_match() );
#endif
#if defined(__GLIBCXX_TYPE_INT_N_1)
static_assert( test( (unsigned __GLIBCXX_TYPE_INT_N_1)1 ) );
static_assert( test( (__GLIBCXX_TYPE_INT_N_1)0 ).did_not_match() );
#endif
#if defined(__GLIBCXX_TYPE_INT_N_2)
static_assert( test( (unsigned __GLIBCXX_TYPE_INT_N_2)1 ) );
static_assert( test( (__GLIBCXX_TYPE_INT_N_2)0 ).did_not_match() );
#endif
#if defined(__GLIBCXX_TYPE_INT_N_3)
static_assert( test( (unsigned __GLIBCXX_TYPE_INT_N_3)1 ) );
static_assert( test( (__GLIBCXX_TYPE_INT_N_3)0 ).did_not_match() );
#endif
