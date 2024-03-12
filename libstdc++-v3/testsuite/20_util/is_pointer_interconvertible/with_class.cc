// { dg-do compile { target c++20 } }
#include <type_traits>

struct A { int i; long l; };

static_assert( std::is_pointer_interconvertible_with_class(&A::i) );
static_assert( ! std::is_pointer_interconvertible_with_class(&A::l) );

constexpr int A::*a = nullptr;
static_assert( ! std::is_pointer_interconvertible_with_class(a) );
static_assert( noexcept( std::is_pointer_interconvertible_with_class(a) ) );

struct B { const int i; };
static_assert( std::is_pointer_interconvertible_with_class(&B::i) );

struct C { int f(); };
static_assert( ! std::is_pointer_interconvertible_with_class(&C::f) );

struct D : A { };
static_assert( std::is_pointer_interconvertible_with_class(&D::i) );

struct E : A { int j; };
// This works because the type of &E::i is int A::* and A is standard-layout:
static_assert( std::is_pointer_interconvertible_with_class(&E::i) );
constexpr int E::*e = a;
// This fails because E is not standard-layout:
static_assert( ! std::is_pointer_interconvertible_with_class(e) );
static_assert( ! std::is_pointer_interconvertible_with_class(&E::j) );
