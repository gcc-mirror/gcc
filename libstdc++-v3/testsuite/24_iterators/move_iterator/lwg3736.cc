// { dg-do compile { target c++20 } }

// 3736.  move_iterator missing disable_sized_sentinel_for specialization

#include <iterator>

template<typename Iter> using MoveIter = std::move_iterator<Iter>;

using std::sized_sentinel_for;
using std::disable_sized_sentinel_for;

// These assertions always passed, even without LWG 3736:
static_assert(sized_sentinel_for<MoveIter<int*>, MoveIter<int*>>);
static_assert(sized_sentinel_for<MoveIter<int*>, MoveIter<const int*>>);
static_assert(not sized_sentinel_for<MoveIter<int*>, MoveIter<long*>>);
static_assert(not sized_sentinel_for<MoveIter<int*>, std::default_sentinel_t>);
static_assert(not disable_sized_sentinel_for<MoveIter<int*>, MoveIter<int*>>);

// These types don't satisfy sized_sentinel_for anyway (because the subtraction
// is ill-formed) but LWG 3736 makes the variable template explicitly false:
static_assert(disable_sized_sentinel_for<MoveIter<int*>, MoveIter<long*>>);

struct Iter
{
  using iterator_category = std::random_access_iterator_tag;
  using value_type = int;
  using pointer = int*;
  using reference = int&;
  using difference_type = long;

  Iter() = default;
  Iter& operator++();
  Iter operator++(int);
  Iter& operator--();
  Iter operator--(int);
  reference operator*() const;
  pointer operator->() const;
  Iter& operator+=(difference_type);
  Iter& operator-=(difference_type);
  friend Iter operator+(Iter, difference_type);
  friend Iter operator+(difference_type, Iter);
  friend Iter operator-(Iter, difference_type);
  friend difference_type operator-(Iter, Iter);
  bool operator==(Iter) const;
};

// Specialize the variable template so that Iter is not its own sized sentinel:
template<> constexpr bool std::disable_sized_sentinel_for<Iter, Iter> = true;
static_assert( not sized_sentinel_for<Iter, Iter> );

// LWG 3736 means that affects std::move_iterator<Iter> as well:
static_assert( not sized_sentinel_for<MoveIter<Iter>, MoveIter<Iter>> );
