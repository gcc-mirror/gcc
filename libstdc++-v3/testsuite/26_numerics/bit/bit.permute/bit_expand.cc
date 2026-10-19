// { dg-do compile { target c++29 } }

#include <bit>
#include <limits>

template<typename UInt>
constexpr auto
test(UInt x)
-> decltype(std::bit_expand(x, x))
{
  constexpr unsigned digits = std::numeric_limits<UInt>::digits;
  static_assert( std::bit_expand(UInt(0), UInt(0)) == UInt(0) );
  static_assert( std::bit_expand(UInt(0x123456789abcdefull), UInt(0)) == UInt(0) );
  static_assert( std::bit_expand(UInt(0x123456789abcdefull), UInt(~UInt(0))) == UInt(0x123456789abcdefull) );
  static_assert( std::bit_expand(UInt(0), UInt(0x123456789abcdefull)) == UInt(0) );
  static_assert( std::bit_expand(UInt(~UInt(0)), UInt(~UInt(0))) == UInt(~UInt(0)) );
  static_assert( std::bit_expand(UInt((UInt(1) << std::popcount(UInt(0x123456789abcdefull))) - 1), UInt(0x123456789abcdefull)) == UInt(0x123456789abcdefull) );
  static_assert( digits < 16 || std::bit_expand(std::bit_repeat(UInt(0x46), 8), std::bit_repeat(UInt(1), 2)) == std::bit_repeat(UInt(0x1014), 16) );
  static_assert( digits < 16 || std::bit_expand(std::bit_repeat(UInt(0x14), 8), std::bit_repeat(UInt(2), 2)) == std::bit_repeat(UInt(0x0220), 16) );
  static_assert( digits < 8 || std::bit_expand(UInt(0xff), UInt(0xff)) == UInt(0xff) );
  static_assert( digits < 8 || std::bit_expand(UInt(0x0f), UInt(0xff)) == UInt(0x0f) );
  static_assert( digits < 8 || std::bit_expand(UInt(0x0f), UInt(0xf0)) == UInt(0xf0) );
  static_assert( std::bit_expand(std::bit_repeat(UInt(2), 2), std::bit_repeat(UInt(0xc), 4)) == std::bit_repeat(UInt(0x8), 4) );
  static_assert( std::bit_expand(std::bit_repeat(UInt(0x0f), 8), std::bit_repeat(UInt(2), 2)) == std::bit_repeat(UInt(0xaa), 16) );
  if constexpr (digits >= 8)
    {
      static_assert( std::bit_expand(UInt(0xd4), UInt(0xbf)) == UInt(0x94) );
      static_assert( std::bit_expand(UInt(0x66), UInt(0x1d)) == UInt(0x0c) );
    }
  if constexpr (digits >= 16)
    {
      static_assert( std::bit_expand(UInt(0xebd4u), UInt(0x93bfu)) == UInt(0x1394) );
      static_assert( std::bit_expand(UInt(0xa166u), UInt(0x001du)) == UInt(0xc) );
    }
  if constexpr (digits >= 32)
    {
      static_assert( std::bit_expand(UInt(0x9daeebd4u), UInt(0xad8793bfu)) == UInt(0xa9851394u) );
      static_assert( std::bit_expand(UInt(0x979ba166u), UInt(0x54ca001du)) == UInt(0x448000cu) );
    }
  if constexpr (digits >= 64)
    {
      static_assert( std::bit_expand(UInt(0x5bd30f069daeebd4ull), UInt(0x64f9a89bad8793bfull)) == UInt(0x4a1a092a9851394ull) );
      static_assert( std::bit_expand(UInt(0xb5672499979ba166ull), UInt(0xd315782354ca001dull)) == UInt(0xd01470200448000cull) );
    }
  return x;
}

static_assert( test( (unsigned char)123 ) );
static_assert( test( (unsigned short)12345 ) );
static_assert( test( (unsigned int)1234567890 ) );
static_assert( test( 123456789UL ) );
static_assert( test( ~123456789ULL ) );

// std::bit_expand(T) shall not participate in overload resolution
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
