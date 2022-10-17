// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }
#include <type_traits>

using std::is_corresponding_member;

struct A { int a; };
struct B { int b; };
struct C: public A, public B { };  // not a standard-layout class

static_assert( is_corresponding_member( &C::a, &C::b ) );
// Succeeds because arguments have types int A::* and int B::*

constexpr int C::*a = &C::a;
constexpr int C::*b = &C::b;
static_assert( ! is_corresponding_member( a, b ) );
// Not corresponding members, because arguments both have type int C::*

static_assert( noexcept(!is_corresponding_member(a, b)) );
