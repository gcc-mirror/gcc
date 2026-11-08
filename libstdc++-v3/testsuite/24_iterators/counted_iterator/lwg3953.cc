// { dg-do compile { target c++20 } }

// LWG 3953. iter_move for common_iterator and counted_iterator should return
// decltype(auto)

// Bug 128413 - common_iterator's iter_move function uses
// iter_rvalue_reference_t<I> as explicit return type

#include <iterator>

struct non_iter_movable_iterator
{
  using value_type      = int;
  using difference_type = int;

  non_iter_movable_iterator() = default;

  int operator*() const { return 0; }

  non_iter_movable_iterator& operator++() { return *this; }
  void operator++(int) {  }

  // iter_move isn't required for output iterators, so this function using that
  // name but returning a non-referenceable type should not cause errors.
  friend void iter_move(const non_iter_movable_iterator&) {}
};

static_assert( std::input_or_output_iterator<non_iter_movable_iterator> );

std::counted_iterator<non_iter_movable_iterator> it;
