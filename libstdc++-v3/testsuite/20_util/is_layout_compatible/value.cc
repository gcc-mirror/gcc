// { dg-do compile { target c++20 } }
// { dg-add-options no_pch }

#include <type_traits>

#ifndef __cpp_lib_is_layout_compatible
# error "Feature test macro for is_layout_compatible is missing in <type_traits>"
#elif __cpp_lib_is_layout_compatible < 201907L
# error "Feature test macro for is_layout_compatible has wrong value in <type_traits>"
#endif

template<typename T, typename U>
concept variable_template_is_correct
  = std::is_layout_compatible_v<T, U> == std::is_layout_compatible<T, U>::value;

template<typename T, typename U>
requires variable_template_is_correct<T, U>
constexpr bool is_layout_compatible = std::is_layout_compatible_v<T, U>;

static_assert( is_layout_compatible<void, void> );
static_assert( is_layout_compatible<int, int> );
static_assert( ! is_layout_compatible<int, int[]> );
static_assert( ! is_layout_compatible<int, int[1]> );
static_assert( is_layout_compatible<int[], int[]> );
static_assert( is_layout_compatible<int[1], int[1]> );
static_assert( ! is_layout_compatible<int[1], int[]> );
static_assert( ! is_layout_compatible<int[1], int[2]> );

struct Incomplete;
// The standard says these are undefined, but they should work really:
// static_assert( is_layout_compatible<Incomplete, Incomplete> );
// static_assert( ! is_layout_compatible<Incomplete[], Incomplete> );
static_assert( is_layout_compatible<Incomplete[], Incomplete[]> );

enum E1 : int { };
enum E2 : int;
static_assert( is_layout_compatible<E1, E2> );
enum E3 : unsigned int;
static_assert( ! is_layout_compatible<E1, E3> );
enum E4 : char { };
enum E5 : signed char { };
enum E6 : unsigned char { };
static_assert( ! is_layout_compatible<E4, E5> );
static_assert( ! is_layout_compatible<E4, E6> );
static_assert( ! is_layout_compatible<E5, E6> );

struct A { int a; };
struct B { const int b; };
static_assert( is_layout_compatible<A, B> );
static_assert( is_layout_compatible<B, A> );

struct C : A { };
struct D : B { };
static_assert( is_layout_compatible<C, D> );

struct E : A { int i; }; // not standard-layout
static_assert( ! is_layout_compatible<E, A> );
