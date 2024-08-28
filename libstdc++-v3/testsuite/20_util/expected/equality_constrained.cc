// { dg-do compile { target c++23 } }

#include <expected>

#ifndef __cpp_lib_constrained_equality
# error "Feature-test macro for constrained_equality missing in <expected>"
#elif __cpp_lib_constrained_equality < 202411L // TODO: use final value
# error "Feature-test macro for constrained_equality has wrong value"
#endif

template<typename T, typename U = T, typename E = int, typename E2 = E>
concept eq_comparable
= requires (const std::expected<T, E>& t, const std::expected<U, E2>& u) {
  t == u;
};

static_assert( eq_comparable<int> );
static_assert( eq_comparable<int, long> );
static_assert( eq_comparable<short, int> );
static_assert( eq_comparable<int, long, unsigned int, unsigned long> );
static_assert( eq_comparable<void, const void> );

struct A { };
static_assert( ! eq_comparable<A> );
static_assert( ! eq_comparable<A, A> );
static_assert( ! eq_comparable<A, int> );
static_assert( ! eq_comparable<int, int, A> );
static_assert( ! eq_comparable<int, int, int, A> );
static_assert( ! eq_comparable<void, const void, A> );

struct B { };
void operator==(B, B);
static_assert( ! eq_comparable<B> );
static_assert( ! eq_comparable<B, B> );
static_assert( ! eq_comparable<B, int> );
static_assert( ! eq_comparable<int, int, B> );
static_assert( ! eq_comparable<int, int, int, B> );
static_assert( ! eq_comparable<void, void, B> );

struct C { };
bool operator==(C, C);
static_assert( eq_comparable<C> );
static_assert( eq_comparable<C, C> );
static_assert( eq_comparable<C, C> );
static_assert( eq_comparable<C, C, C> );
static_assert( eq_comparable<int, int, C> );
static_assert( ! eq_comparable<C, C, A, A> );
static_assert( ! eq_comparable<C, C, A, int> );
static_assert( eq_comparable<void, void, C> );

struct D { };
int operator==(D, D);
bool operator!=(D, D) = delete;
static_assert( eq_comparable<D> );
static_assert( eq_comparable<D, D> );
static_assert( eq_comparable<int, int, D> );
static_assert( eq_comparable<D, D, D> );
static_assert( ! eq_comparable<D, D, D, int> );

struct E { };
bool operator==(/* not-const */ E&, const E&);
static_assert( ! eq_comparable<E> );
static_assert( ! eq_comparable<E, E> );
static_assert( ! eq_comparable<E, E> );
static_assert( ! eq_comparable<int, int, E> );

bool operator==(A, B);
static_assert( eq_comparable<A, B> );
static_assert( eq_comparable<B, A> );
static_assert( eq_comparable<int, long, B, A> );

int operator==(C, D);
static_assert( eq_comparable<C, D> );
static_assert( eq_comparable<D, C> );
static_assert( eq_comparable<int, int, C, D> );
static_assert( eq_comparable<int, int, D, C> );
static_assert( eq_comparable<const void, void, C, D> );
static_assert( eq_comparable<const void, void, D, C> );

template<typename T, typename U = T, typename E = int>
concept eq_comparable_val
= requires (const std::expected<T, E>& t, const U& u) {
  t == u;
};

static_assert( eq_comparable_val<int> );
static_assert( eq_comparable_val<int, long> );
static_assert( eq_comparable_val<short, int> );
static_assert( eq_comparable_val<int, long, unsigned> );
static_assert( ! eq_comparable_val<void, const void> );

static_assert( ! eq_comparable_val<A> );
static_assert( ! eq_comparable_val<A, A> );
static_assert( ! eq_comparable_val<A, int> );
static_assert( eq_comparable_val<int, int, A> );
static_assert( ! eq_comparable_val<void, A> );

static_assert( ! eq_comparable_val<B> );
static_assert( ! eq_comparable_val<B, B> );
static_assert( ! eq_comparable_val<B, int> );
static_assert( eq_comparable_val<int, int, B> );
static_assert( ! eq_comparable_val<void, B> );

static_assert( eq_comparable_val<C> );
static_assert( eq_comparable_val<C, C> );
static_assert( eq_comparable_val<C, C> );
static_assert( eq_comparable_val<C, C, C> );
static_assert( eq_comparable_val<int, int, C> );
static_assert( eq_comparable_val<C, C, A> );
static_assert( ! eq_comparable_val<void, C> );

static_assert( eq_comparable_val<D> );
static_assert( eq_comparable_val<D, D> );
static_assert( ! eq_comparable_val<D, int> );
static_assert( eq_comparable_val<int, int, D> );
static_assert( eq_comparable_val<D, D, D> );

static_assert( ! eq_comparable_val<E> );
static_assert( ! eq_comparable_val<E, E> );
static_assert( ! eq_comparable_val<E, int> );
static_assert( eq_comparable_val<int, int, E> );

static_assert( eq_comparable_val<A, B> );
static_assert( eq_comparable_val<B, A> );

static_assert( eq_comparable_val<C, D> );
static_assert( ! eq_comparable_val<D, C> );

template<typename E, typename U, typename T = int>
concept eq_comparable_unex
= requires (const std::expected<T, E>& t, const std::unexpected<U>& u) {
  t == u;
};

static_assert( eq_comparable_unex<int, int> );
static_assert( eq_comparable_unex<int, long> );
static_assert( eq_comparable_unex<short, int> );
static_assert( eq_comparable_unex<int, int, void> );
static_assert( eq_comparable_unex<int, long, void> );
static_assert( eq_comparable_unex<short, int, void> );

static_assert( ! eq_comparable_unex<A, A> );
static_assert( ! eq_comparable_unex<A, int> );
static_assert( ! eq_comparable_unex<int, A> );
static_assert( ! eq_comparable_unex<A, A, void> );
static_assert( ! eq_comparable_unex<A, int, void> );
static_assert( ! eq_comparable_unex<int, A, void> );

static_assert( ! eq_comparable_unex<B, B> );
static_assert( ! eq_comparable_unex<B, int> );
static_assert( ! eq_comparable_unex<int, B> );
static_assert( ! eq_comparable_unex<B, B, void> );
static_assert( ! eq_comparable_unex<B, int, void> );
static_assert( ! eq_comparable_unex<int, B, void> );

static_assert( eq_comparable_unex<C, C> );
static_assert( eq_comparable_unex<C, C, void> );

static_assert( eq_comparable_unex<D, D> );
static_assert( ! eq_comparable_unex<D, int> );
static_assert( ! eq_comparable_unex<int, D> );
static_assert( eq_comparable_unex<D, D, void> );
static_assert( ! eq_comparable_unex<D, int, void> );
static_assert( ! eq_comparable_unex<int, D, void> );

static_assert( ! eq_comparable_unex<E, E> );
static_assert( ! eq_comparable_unex<E, int> );
static_assert( ! eq_comparable_unex<int, E> );
static_assert( ! eq_comparable_unex<E, E, void> );
static_assert( ! eq_comparable_unex<E, int, void> );
static_assert( ! eq_comparable_unex<int, E, void> );

static_assert( eq_comparable_unex<A, B> );
static_assert( eq_comparable_unex<B, A> );
static_assert( eq_comparable_unex<A, B, void> );
static_assert( eq_comparable_unex<B, A, void> );

static_assert( eq_comparable_unex<C, D> );
static_assert( ! eq_comparable_unex<D, C> );
static_assert( eq_comparable_unex<C, D, void> );
static_assert( ! eq_comparable_unex<D, C, void> );
