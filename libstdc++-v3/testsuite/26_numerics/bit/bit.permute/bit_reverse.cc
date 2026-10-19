// { dg-do compile { target c++29 } }

#include <bit>
#include <limits>

template<typename UInt>
constexpr auto
test(UInt x)
-> decltype(std::bit_reverse(x))
{
  constexpr unsigned digits = std::numeric_limits<UInt>::digits;
  static_assert( std::bit_reverse(UInt(0)) == UInt(0) );
  static_assert( std::bit_reverse(UInt(~UInt(0))) == UInt(~UInt(0)) );
  static_assert( std::bit_reverse(UInt(1)) == UInt(1) << (digits - 1) );
  static_assert( std::bit_reverse(UInt(UInt(1) << (digits - 1))) == UInt(1) );
  if constexpr (std::numeric_limits<UInt>::digits == 8)
    static_assert( std::bit_reverse<UInt>(0x34u) == 0x2cu );
  if constexpr (std::numeric_limits<UInt>::digits == 16)
    static_assert( std::bit_reverse<UInt>(0x1234u) == 0x2c48u );
  if constexpr (std::numeric_limits<UInt>::digits == 32)
    static_assert( std::bit_reverse<UInt>(0x01234567u) == 0xe6a2c480u );
  if constexpr (std::numeric_limits<UInt>::digits == 64)
    static_assert( std::bit_reverse<UInt>(0x0123456789abcdefull) == 0xf7b3d591e6a2c480ull );
  if constexpr (std::numeric_limits<UInt>::digits == 128)
    static_assert( std::bit_reverse<UInt>((UInt(0x0123456789abcdefull) << 64)
					  | 0x2468ace013579bdfull)
		   == ((UInt(0xfbd9eac807351624ull) << 64)
		       | 0xf7b3d591e6a2c480ull) );
  if (std::bit_reverse(std::bit_reverse(x)) == x)
    return x;
  return 0;
}

static_assert( test( (unsigned char)123 ) );
static_assert( test( (unsigned short)12345 ) );
static_assert( test( (unsigned int)1234567890 ) );
static_assert( test( 123456789UL ) );
static_assert( test( ~123456789ULL ) );

// std::bit_reverse(T) shall not participate in overload resolution
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
