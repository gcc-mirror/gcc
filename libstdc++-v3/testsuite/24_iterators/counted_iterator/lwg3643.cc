// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }

#include <iterator>

struct InputIterator
{
  using difference_type = int;
  using value_type = int;

  constexpr int operator*() const noexcept { return 0; }
  InputIterator& operator++() { return *this; }
  constexpr void operator++(int) { }
};

static_assert( std::input_iterator<InputIterator> );
static_assert( !std::forward_iterator<InputIterator> );

constexpr bool
test_lwg3643()
{
  std::counted_iterator<InputIterator> iter({}, 1);
  iter++;
  return iter == std::default_sentinel;
}

static_assert( test_lwg3643() );
