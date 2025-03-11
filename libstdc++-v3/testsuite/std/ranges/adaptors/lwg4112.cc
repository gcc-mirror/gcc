// { dg-do compile { target c++20 } }

// LWG 4112. has-arrow should required operator->() to be const-qualified

// The issue resolution means that range adaptors which use has-arrow to
// constrain their iterator's operator-> should require a const-qualified
// operator-> on the underlying view's iterator.

#include <ranges>

struct Int { int i = 0; };

struct Iter
{
  using value_type = Int;
  using difference_type = int;

  mutable Int val;

  Int& operator*() const { return val; }
  Int* operator->() /* non-const */ { return &val; }
  Iter& operator++() { ++val.i; return *this; }
  void operator++(int) { ++val.i; }
  bool operator==(const Iter& j) const { return val.i == j.val.i; }
};

template<typename T>
concept has_op_arrow = requires (T t) { t.operator->(); };

static_assert( has_op_arrow<Iter> );
static_assert( ! has_op_arrow<const Iter> );

using Range = std::ranges::subrange<Iter>;
using Pred = bool(*)(Int);
using FilterView = std::ranges::filter_view<Range, Pred>;
using FilterIterator = std::ranges::iterator_t<FilterView>;

static_assert( ! has_op_arrow<FilterIterator> );
static_assert( ! has_op_arrow<FilterIterator&> );
static_assert( ! has_op_arrow<FilterIterator const> );
static_assert( ! has_op_arrow<FilterIterator const&> );
