// { dg-do run { target c++20 } }

#include <ranges>
#include <algorithm>
#include <regex>
#include <string_view>
#include <testsuite_hooks.h>

namespace ranges = std::ranges;
namespace views = std::views;

void
test01()
{
  // Test case from LWG 3698
  char const text[] = "Hello";
  std::regex regex{"[a-z]"};

  auto lower
    = ranges::subrange(std::cregex_iterator(ranges::begin(text),
					    ranges::end(text),
					    regex),
		       std::cregex_iterator{})
    | views::join
    | views::transform([](auto const& sm) {
      return std::string_view(sm.first, sm.second);
    });

  VERIFY( ranges::equal(lower, (std::string_view[]){"e", "l", "l", "o"}));
}

void
test02()
{
#if __cpp_lib_ranges_join_with
  // Analogous test case from LWG 3698 for join_with_view
  char const text[] = "Hello";
  std::regex regex{"[a-z]"};

  auto lower
    = ranges::subrange(std::cregex_iterator(ranges::begin(text),
					    ranges::end(text),
					    regex),
		       std::cregex_iterator{})
    | views::join_with(views::empty<std::sub_match<const char*>>)
    | views::transform([](auto const& sm) {
      return std::string_view(sm.first, sm.second);
    });

  VERIFY( ranges::equal(lower, (std::string_view[]){"e", "l", "l", "o"}));
#endif
}

void
test03()
{
  // Test case from LWG 3700
  auto r = views::iota(0, 5) | views::split(1);
  auto s = views::single(r);
  auto j = s | views::join;
  auto f = j.front();
}

void
test04()
{
#if __cpp_lib_ranges_join_with
  // Analogous test case from LWG 3700 for join_with_view
  auto r = views::iota(0, 5) | views::split(1);
  auto s = views::single(r);
  auto j = s | views::join_with(views::empty<ranges::range_value_t<decltype(r)>>);
  auto f = j.front();
#endif
}

void
test05()
{
  // Test case from LWG 3791
  std::vector<std::vector<int>> v = {{1}};
  auto r = v
    | views::transform([](auto& x) -> auto&& { return std::move(x); })
    | views::join;
  auto e = --r.end();
}

void
test06()
{
#if __cpp_lib_ranges_join_with
  // Analogous test case from LWG 3791 for join_with_view
  std::vector<std::vector<int>> v = {{1}};
  auto r = v
    | views::transform([](auto& x) -> auto&& { return std::move(x); })
    | views::join_with(views::empty<int>);
  auto e = --r.end();
#endif
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
}
