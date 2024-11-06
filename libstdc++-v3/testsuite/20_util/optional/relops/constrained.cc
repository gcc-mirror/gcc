// { dg-do compile { target c++20 } }

#include <optional>

#ifndef __cpp_lib_constrained_equality
# error "Feature-test macro for constrained_equality missing"
#elif __cpp_lib_constrained_equality < 202403L
# error "Feature-test macro for constrained_equality has wrong value"
#endif

template<typename T, typename U = T>
concept eq_comparable
= requires (const std::optional<T>& t, const std::optional<U>& u) {
  t == u;
  *t == u;
  t == *u;
};

template<typename T, typename U = T>
concept ne_comparable
= requires (const std::optional<T>& t, const std::optional<U>& u) {
  t != u;
  *t != u;
  t != *u;
};

template<typename T, typename U = T>
concept lt_comparable
= requires (const std::optional<T>& t, const std::optional<U>& u) {
  t < u;
  *t < u;
  t < *u;
};

template<typename T, typename U = T>
concept le_comparable
= requires (const std::optional<T>& t, const std::optional<U>& u) {
  t <= u;
  *t <= u;
  t <= *u;
};

template<typename T, typename U = T>
concept gt_comparable
= requires (const std::optional<T>& t, const std::optional<U>& u) {
  t > u;
  *t > u;
  t > *u;
};

template<typename T, typename U = T>
concept ge_comparable
= requires (const std::optional<T>& t, const std::optional<U>& u) {
  t >= u;
  *t >= u;
  t >= *u;
};

static_assert( eq_comparable<int> );
static_assert( ne_comparable<int> );
static_assert( lt_comparable<int> );
static_assert( le_comparable<int> );
static_assert( gt_comparable<int> );
static_assert( ge_comparable<int> );
static_assert( eq_comparable<int, long> );
static_assert( ne_comparable<int, long> );
static_assert( lt_comparable<int, long> );
static_assert( le_comparable<int, long> );
static_assert( gt_comparable<int, long> );
static_assert( ge_comparable<int, long> );
static_assert( eq_comparable<short, int> );
static_assert( ne_comparable<short, int> );
static_assert( lt_comparable<short, int> );
static_assert( le_comparable<short, int> );
static_assert( gt_comparable<short, int> );
static_assert( ge_comparable<short, int> );

struct A { };
static_assert( ! eq_comparable<A> );
static_assert( ! ne_comparable<A> );
static_assert( ! lt_comparable<A> );
static_assert( ! le_comparable<A> );
static_assert( ! gt_comparable<A> );
static_assert( ! ge_comparable<A> );
static_assert( ! eq_comparable<A, A> );
static_assert( ! ne_comparable<A, A> );
static_assert( ! lt_comparable<A, A> );
static_assert( ! le_comparable<A, A> );
static_assert( ! gt_comparable<A, A> );
static_assert( ! ge_comparable<A, A> );
static_assert( ! eq_comparable<A, int> );
static_assert( ! ne_comparable<A, int> );
static_assert( ! lt_comparable<A, int> );
static_assert( ! le_comparable<A, int> );
static_assert( ! gt_comparable<A, int> );
static_assert( ! ge_comparable<A, int> );

struct B { };
void operator==(B, B);
static_assert( ! eq_comparable<B> );
static_assert( ! ne_comparable<B> );
static_assert( ! lt_comparable<B> );
static_assert( ! le_comparable<B> );
static_assert( ! gt_comparable<B> );
static_assert( ! ge_comparable<B> );
static_assert( ! eq_comparable<B, B> );
static_assert( ! ne_comparable<B, B> );
static_assert( ! lt_comparable<B, B> );
static_assert( ! le_comparable<B, B> );
static_assert( ! gt_comparable<B, B> );
static_assert( ! ge_comparable<B, B> );
static_assert( ! eq_comparable<B, int> );
static_assert( ! ne_comparable<B, int> );
static_assert( ! lt_comparable<B, int> );
static_assert( ! le_comparable<B, int> );
static_assert( ! gt_comparable<B, int> );
static_assert( ! ge_comparable<B, int> );

struct C { };
bool operator==(C, C);
static_assert( eq_comparable<C> );
static_assert( ne_comparable<C> );
static_assert( ! lt_comparable<C> );
static_assert( ! le_comparable<C> );
static_assert( ! gt_comparable<C> );
static_assert( ! ge_comparable<C> );
static_assert( eq_comparable<C, C> );
static_assert( ne_comparable<C, C> );
static_assert( eq_comparable<C, C> );
static_assert( ne_comparable<C, C> );

struct D { };
int operator==(D, D);
bool operator!=(D, D) = delete;
static_assert( eq_comparable<D> );
static_assert( ! ne_comparable<D> );
static_assert( ! lt_comparable<D> );
static_assert( ! le_comparable<D> );
static_assert( ! gt_comparable<D> );
static_assert( ! ge_comparable<D> );
static_assert( eq_comparable<D, D> );
static_assert( ! ne_comparable<D, D> );
static_assert( eq_comparable<D, D> );
static_assert( ! ne_comparable<D, D> );

struct E { };
bool operator==(/* not-const */ E&, const E&);
static_assert( ! eq_comparable<E> );
static_assert( ! ne_comparable<E> );
static_assert( ! lt_comparable<E> );
static_assert( ! le_comparable<E> );
static_assert( ! gt_comparable<E> );
static_assert( ! ge_comparable<E> );
static_assert( ! eq_comparable<E, E> );
static_assert( ! eq_comparable<E, E> );

struct F { };
bool operator<(F, F);
void operator>(F, F);
static_assert( ! eq_comparable<F> );
static_assert( ! ne_comparable<F> );
static_assert( lt_comparable<F> );
static_assert( ! le_comparable<F> );
static_assert( ! gt_comparable<F> );
static_assert( ! ge_comparable<F> );
static_assert( lt_comparable<F, F> );
static_assert( lt_comparable<F, F> );

struct G { };
bool operator<=(G, G);
void operator<(G, G);
static_assert( ! eq_comparable<G> );
static_assert( ! ne_comparable<G> );
static_assert( ! lt_comparable<G> );
static_assert( le_comparable<G> );
static_assert( ! gt_comparable<G> );
static_assert( ! ge_comparable<G> );
static_assert( le_comparable<G, G> );
static_assert( le_comparable<G, G> );

struct H { };
bool operator>(H, H);
void operator>=(H, H);
static_assert( ! eq_comparable<H> );
static_assert( ! ne_comparable<H> );
static_assert( ! lt_comparable<H> );
static_assert( ! le_comparable<H> );
static_assert( gt_comparable<H> );
static_assert( ! ge_comparable<H> );
static_assert( gt_comparable<H, H> );
static_assert( gt_comparable<H, H> );

struct I { };
bool operator>=(I, I);
void operator<=(I, I);
static_assert( ! eq_comparable<I> );
static_assert( ! ne_comparable<I> );
static_assert( ! lt_comparable<I> );
static_assert( ! le_comparable<I> );
static_assert( ! gt_comparable<I> );
static_assert( ge_comparable<I> );
static_assert( ge_comparable<I, I> );
static_assert( ge_comparable<I, I> );

struct J { };
bool operator==(J, J);
std::weak_ordering operator<=>(J, J);
static_assert( eq_comparable<J> );
static_assert( ne_comparable<J> );
static_assert( lt_comparable<J> );
static_assert( le_comparable<J> );
static_assert( gt_comparable<J> );
static_assert( ge_comparable<J> );

struct K { };
int operator==(K, K); // non-bool prevents synthesis of !=
void operator<=(K, K);
std::weak_ordering operator<=>(K, K);
static_assert( eq_comparable<K> );
static_assert( ! ne_comparable<K> );
static_assert( lt_comparable<K> );
static_assert( ! le_comparable<K> );
static_assert( gt_comparable<K> );
static_assert( ge_comparable<K> );

bool operator==(A, B);
static_assert( eq_comparable<A, B> );
static_assert( eq_comparable<B, A> );
static_assert( ne_comparable<A, B> );
static_assert( ne_comparable<B, A> );
static_assert( ! lt_comparable<A, B> );
static_assert( ! le_comparable<A, B> );
static_assert( ! gt_comparable<A, B> );
static_assert( ! ge_comparable<A, B> );

int operator==(C, D); // non-bool prevents synthesis of != and reversed args
static_assert( eq_comparable<C, D> );
static_assert( ! eq_comparable<D, C> );
static_assert( ! ne_comparable<C, D> );
static_assert( ! ne_comparable<D, C> );
static_assert( ! lt_comparable<C, D> );
static_assert( ! le_comparable<C, D> );
static_assert( ! gt_comparable<C, D> );
static_assert( ! ge_comparable<C, D> );

std::weak_ordering operator<=>(E, F);
static_assert( ! eq_comparable<E, F> );
static_assert( ! eq_comparable<F, E> );
static_assert( ! ne_comparable<E, F> );
static_assert( ! ne_comparable<F, E> );
static_assert( lt_comparable<E, F> );
static_assert( le_comparable<E, F> );
static_assert( gt_comparable<E, F> );
static_assert( ge_comparable<E, F> );
static_assert( lt_comparable<F, E> );
static_assert( le_comparable<F, E> );
static_assert( gt_comparable<F, E> );
static_assert( ge_comparable<F, E> );
