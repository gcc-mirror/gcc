// { dg-do compile { target c++20 } }

#include <utility>

#ifndef __cpp_lib_constrained_equality
# error "Feature-test macro for constrained_equality missing"
#elif __cpp_lib_constrained_equality < 202403L
# error "Feature-test macro for constrained_equality has wrong value"
#endif

template<typename T>
concept equality_comparable = requires (const T& t) { t == t; t != t; };

static_assert( equality_comparable<std::pair<int, long>> );

struct A { };
static_assert( ! equality_comparable<std::pair<A, long>> );
static_assert( ! equality_comparable<std::pair<int, A>> );
static_assert( ! equality_comparable<std::pair<A, A>> );

struct B { };
void operator==(B, B);
static_assert( ! equality_comparable<std::pair<B, long>> );
static_assert( ! equality_comparable<std::pair<int, B>> );
static_assert( ! equality_comparable<std::pair<B, B>> );
static_assert( ! equality_comparable<std::pair<A, B>> );

struct C { };
int operator==(C, C);
static_assert( equality_comparable<std::pair<C, long>> );
static_assert( equality_comparable<std::pair<int, C>> );
static_assert( equality_comparable<std::pair<C, C>> );
static_assert( ! equality_comparable<std::pair<A, C>> );

struct D { };
bool operator==(D, D);
bool operator!=(D, D) = delete;
static_assert( equality_comparable<std::pair<D, long>> );
static_assert( equality_comparable<std::pair<int, D>> );
static_assert( equality_comparable<std::pair<D, D>> );
static_assert( equality_comparable<std::pair<C, D>> );
static_assert( ! equality_comparable<std::pair<A, C>> );

struct E { };
bool operator==(/* not-const */ E&, const E&);
static_assert( ! equality_comparable<std::pair<E, long>> );
static_assert( ! equality_comparable<std::pair<int, E>> );
static_assert( ! equality_comparable<std::pair<E, E>> );
