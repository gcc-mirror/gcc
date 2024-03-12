// { dg-do compile { target c++20 } }

#include <compare>

using std::strong_ordering;
using std::partial_ordering;

namespace adl
{
  struct S { };
  void strong_ordering(const S&, const S&);
  bool operator==(const S&, S&) { return true; }
  bool operator<(const S&, S&) { return true; }
}

template<typename T, typename U>
  concept has_strong_order_fallback = requires (T& t, U& u) {
    std::compare_strong_order_fallback(t, u);
  };

template<typename T, typename U>
  concept has_weak_order_fallback = requires (T& t, U& u) {
    std::compare_weak_order_fallback(t, u);
  };

template<typename T, typename U>
  concept has_partial_order_fallback = requires (T& t, U& u) {
    std::compare_partial_order_fallback(t, u);
  };

using adl::S;

static_assert( ! has_strong_order_fallback<S, S> );
static_assert( has_strong_order_fallback<const S, S> );
static_assert( ! has_strong_order_fallback<const S, const S> );
static_assert( ! has_weak_order_fallback<S, S> );
static_assert( has_weak_order_fallback<const S, S> );
static_assert( ! has_weak_order_fallback<const S, const S> );
static_assert( ! has_partial_order_fallback<S, S> );
static_assert( ! has_partial_order_fallback<const S, S> ); // LWG 3465
static_assert( ! has_partial_order_fallback<const S, const S> );
