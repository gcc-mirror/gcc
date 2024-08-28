// { dg-do compile { target c++20 } }

#include <tuple>

#ifndef __cpp_lib_constrained_equality
# error "Feature-test macro for constrained_equality missing"
#elif __cpp_lib_constrained_equality < 202403L
# error "Feature-test macro for constrained_equality has wrong value"
#endif

template<typename T>
concept equality_comparable = requires (const T& t) { t == t; t != t; };

static_assert( equality_comparable<std::tuple<>> );
static_assert( equality_comparable<std::tuple<int>> );
static_assert( equality_comparable<std::tuple<int, long>> );
static_assert( equality_comparable<std::tuple<int, long, short>> );

struct A { };
static_assert( ! equality_comparable<std::tuple<A>> );
static_assert( ! equality_comparable<std::tuple<int, A>> );
static_assert( ! equality_comparable<std::tuple<A, A>> );

struct B { };
void operator==(B, B);
static_assert( ! equality_comparable<std::tuple<B>> );
static_assert( ! equality_comparable<std::tuple<int, B>> );
static_assert( ! equality_comparable<std::tuple<B, B>> );

struct C { };
int operator==(C, C);
static_assert( equality_comparable<std::tuple<C>> );
static_assert( equality_comparable<std::tuple<int, C>> );
static_assert( equality_comparable<std::tuple<C, C>> );
static_assert( ! equality_comparable<std::tuple<A, C>> );

struct D { };
bool operator==(D, D);
bool operator!=(D, D) = delete;
static_assert( equality_comparable<std::tuple<D>> );
static_assert( equality_comparable<std::tuple<int, D>> );
static_assert( equality_comparable<std::tuple<D, D>> );
static_assert( equality_comparable<std::tuple<C, D>> );
static_assert( ! equality_comparable<std::tuple<A, C>> );

struct E { };
bool operator==(/* not-const */ E&, const E&);
static_assert( ! equality_comparable<std::tuple<E>> );
static_assert( ! equality_comparable<std::tuple<int, E>> );
static_assert( ! equality_comparable<std::tuple<E, E>> );
