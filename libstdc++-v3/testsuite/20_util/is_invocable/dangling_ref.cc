// { dg-do compile { target c++17 } }
#include <type_traits>

static_assert( not std::is_invocable_r_v<const int&, int()> );
static_assert( not std::is_invocable_r_v<int&&, int()> );
static_assert( not std::is_invocable_r_v<const long&, int()> );
