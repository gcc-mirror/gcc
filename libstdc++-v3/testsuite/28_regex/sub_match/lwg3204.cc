// { dg-do run { target c++11 } }
#include <regex>
#include <testsuite_hooks.h>

// LWG 3204. sub_match::swap only swaps the base class

int main()
{
  std::sub_match<const char*> a, b;
  a.matched = true;
  a.swap(b);
  VERIFY( ! a.matched );
  VERIFY( b.matched );
}

struct iter
{
  using value_type = char;
  using difference_type = long;
  using pointer = const char*;
  using reference = const char&;
  using iterator_category = std::bidirectional_iterator_tag;

  iter();
  iter(const iter&) noexcept(false);

  iter& operator++();
  iter operator++(int);
  iter& operator--();
  iter operator--(int);
  reference operator*() const;
  pointer operator->() const;
};

using CS = std::csub_match;
static_assert( noexcept(std::declval<CS&>().swap(std::declval<CS&>())) );
using IS = std::sub_match<iter>;
static_assert( ! noexcept(std::declval<IS&>().swap(std::declval<IS&>())) );
