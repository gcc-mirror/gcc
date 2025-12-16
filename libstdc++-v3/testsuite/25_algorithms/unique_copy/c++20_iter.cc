// Verify std::unique_copy is C++20 iterator aware as per P2408R5.
// { dg-do compile { target c++20 } }

#include <algorithm>
#include <ranges>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

struct noncopyable
{
  constexpr operator int() { return 42; }
  noncopyable() = default;
  noncopyable(const noncopyable&) = delete;
  noncopyable& operator=(const noncopyable&) = delete;
  friend auto operator<=>(const noncopyable&, const noncopyable&) = default;
};

constexpr bool
test01()
{
  auto r = std::views::iota(10)
    | std::views::transform([](int) { return noncopyable{}; });
  auto it = r.begin();
  static_assert( std::random_access_iterator<decltype(it)>);
  static_assert( std::same_as<std::iterator_traits<decltype(it)>::iterator_category,
			      std::input_iterator_tag> );
  int buf[10];
  __gnu_test::input_container<int> s(buf);
  auto jt = std::unique_copy(it, it+10, s.begin());
  return true;
}

static_assert(test01());
