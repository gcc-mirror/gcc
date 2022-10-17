// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }

#include <iterator>

struct movable_iterator
{
  using difference_type = long;

  movable_iterator() = default;
  movable_iterator(movable_iterator&&) = default;
  movable_iterator& operator=(movable_iterator&&) = default;

  int operator*() const { return 1; }

  movable_iterator& operator++() { return *this; }
  void operator++(int) { }

  bool operator==(const movable_iterator&) const = default;
};

using namespace std;

constexpr counted_iterator<movable_iterator> it({}, 3);

static_assert( sized_sentinel_for<std::default_sentinel_t, counted_iterator<movable_iterator>> );
// LWG 3392
// ranges::distance() cannot be used on a move-only iterator
// with a sized sentinel
static_assert( ranges::distance(it, default_sentinel) == 3 );
