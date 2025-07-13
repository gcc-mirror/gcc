// { dg-do compile { target c++11 } }
// { dg-add-options strict_std }

#include <type_traits>

#ifdef __SIZEOF_INT128__
enum E : __int128 { };
using U = std::make_unsigned<E>::type;
static_assert( std::is_integral<U>::value, "type is an integer" );
static_assert( sizeof(U) == sizeof(E), "width of type is 128 bits" );
using I = std::make_signed<E>::type;
static_assert( std::is_integral<I>::value, "type is an integer" );
static_assert( sizeof(I) == sizeof(E), "width of type is 128 bits" );
#endif
