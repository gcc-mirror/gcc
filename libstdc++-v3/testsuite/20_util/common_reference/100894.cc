// { dg-do compile { target c++20 } }
// PR libstdc++/100894 - common_reference implementation seems to be wrong

#include <type_traits>

struct A {};
struct B { B(A); };
static_assert( std::is_same_v<std::common_reference_t<A&, B&&>, B> );
