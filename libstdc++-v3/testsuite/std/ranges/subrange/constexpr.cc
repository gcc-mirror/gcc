// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }

#include <ranges>

struct iterator
{
  using difference_type = int;

  int i;

  int operator*() const { return i; }

  // These are intentionally not constexpr:
  iterator& operator++() { ++i; return *this; }
  iterator operator++(int) { return {i++}; }
  bool operator==(const iterator& it) const { return i == it.i; }
};

constexpr iterator begin(1), end(2);

using std::ranges::subrange;
using std::ranges::subrange_kind;

// This used to fail due to using operator++ and operator== in an assertion:
constexpr subrange<iterator, iterator, subrange_kind::sized> s(begin, end, 1);
