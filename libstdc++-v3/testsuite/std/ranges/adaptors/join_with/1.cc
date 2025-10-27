// { dg-do run { target c++23 } }
// { dg-add-options no_pch }

#include <ranges>

#if __cpp_lib_ranges_join_with != 202202L
# error "Feature-test macro __cpp_lib_ranges_join_with has wrong value in <ranges>"
#endif

#include <algorithm>
#include <string>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;
namespace views = std::views;
using namespace std::literals;

constexpr bool
test01()
{
  std::string_view rs[] = {"hello", "world"};
  auto v = rs | views::join_with(' ');
  VERIFY( ranges::equal(v | views::split(' '), rs, ranges::equal) );
  auto i = v.begin(), j = v.begin();
  VERIFY( i == j );
  ++i;
  i++;
  VERIFY( i != j );
  VERIFY( *i == 'l' );
  --i;
  i--;
  VERIFY( *i == 'h' );
  return true;
}

constexpr bool
test02()
{
  std::string_view rs[] = {"the", "quick", "brown", "fox"};
  auto v = rs
    | views::transform([](auto x) { return x; })
    | views::filter([](auto) { return true; });
  VERIFY( ranges::equal(v | views::join_with(views::empty<char>), "thequickbrownfox"sv) );
  VERIFY( ranges::equal(v | views::join_with('-'), "the-quick-brown-fox"sv) );
  VERIFY( ranges::equal(v | views::join_with("--"sv), "the--quick--brown--fox"sv) );
  VERIFY( ranges::empty(views::empty<int[3]> | views::join_with(0)));
  VERIFY( ranges::equal(views::single(std::array{42}) | views::join_with(0), (int[]){42}));
  return true;
}

constexpr bool
test03()
{
  using __gnu_test::test_input_range;
  using __gnu_test::test_forward_range;
  using __gnu_test::test_bidirectional_range;

  using ty1 = ranges::join_with_view<views::all_t<test_input_range<test_input_range<int>>>,
				     views::all_t<test_forward_range<int>>>;
  static_assert(ranges::input_range<ty1>);
  static_assert(!ranges::forward_range<ty1>);
  static_assert(!ranges::common_range<ty1>);

  using ty2 = ranges::join_with_view<views::all_t<test_forward_range<test_forward_range<int>>>,
				     views::all_t<test_forward_range<int>>>;
  static_assert(ranges::forward_range<ty2>);
  static_assert(!ranges::bidirectional_range<ty2>);
  static_assert(!ranges::common_range<ty2>);

  using ty3 = ranges::join_with_view<views::all_t<std::array<std::string_view, 3>>,
				     std::string_view>;
  static_assert(ranges::bidirectional_range<ty3>);
  static_assert(!ranges::random_access_range<ty3>);
  static_assert(ranges::common_range<ty3>);

  return true;
}

#if _GLIBCXX_USE_CXX11_ABI
constexpr
#endif
bool
test04()
{
  std::string rs[] = {"a", "", "b", "", "c"};
  auto v = rs | views::join_with(' ');
  VERIFY( ranges::equal(v, "a  b  c"sv) );
  auto i = v.begin();
  auto j = ranges::next(i, 3);
  ranges::iter_swap(i, j);
  *j = ranges::iter_move(i);
  VERIFY( ranges::equal(v, "b  b  c"sv) );
  return true;
}

void
test05()
{
  // PR libstdc++/119962 - __maybe_present_t misses initialization
  constexpr decltype(views::join_with(views::single(views::single(0)), 0).begin()) it;
}

int
main()
{
  static_assert(test01());
  static_assert(test02());
  static_assert(test03());
#if _GLIBCXX_USE_CXX11_ABI
  static_assert(test04());
#else
  VERIFY(test04());
#endif
  test05();
}
