// { dg-do run { target c++23 } }

#include <ranges>
#include <algorithm>
#include <utility>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;
namespace views = std::views;

template<typename T>
concept can_zip_transform = requires (T t) {
  views::zip_transform(std::forward<T>(t));
};

static_assert(!can_zip_transform<int>);

struct NonMovable {
  NonMovable(NonMovable&&) = delete;
};

static_assert(!can_zip_transform<NonMovable>);
static_assert(!can_zip_transform<NonMovable&>);

static_assert(!can_zip_transform<void(*)()>);
static_assert(can_zip_transform<int(&(*)())[3]>);

constexpr bool
test01()
{
  static_assert(ranges::empty(views::zip_transform([] { return 0; })));

  auto z1 = views::zip_transform(std::identity{},
				 std::array{1, 2, 3});
  VERIFY( ranges::equal(z1, (int[]){1, 2, 3}) );
  const auto i0 = z1.begin(), i1 = z1.begin() + 1;
  VERIFY( i0 + 1 - 1 == i0 );
  VERIFY( i0 < i1 );
  VERIFY( i1 < z1.end() );
  VERIFY( i1 - i0 == 1 );
  VERIFY( i0 - i1 == -1 );
  VERIFY( z1.end() - i1 == 2 );
  VERIFY( i1 - z1.end() == -2 );
  ranges::iter_swap(i0, i1);
  VERIFY( ranges::equal(std::move(z1), (int[]){2, 1, 3}) );

  auto z2 = views::zip_transform(std::multiplies{},
				 std::array{-1, 2},
				 std::array{3, 4, 5});
  auto i2 = z2.begin();
  i2 += 1;
  i2 -= -1;
  VERIFY( i2 == z2.end() );
  VERIFY( ranges::size(z2) == 2 );
  VERIFY( ranges::size(std::as_const(z2)) == 2 );
  VERIFY( ranges::equal(z2, (int[]){-3, 8}) );

  auto z3 = views::zip_transform([] (auto... xs) { return ranges::max({xs...}); },
				 std::array{1, 6, 7, 0, 0},
				 std::array{2, 5, 9},
				 std::array{3, 4, 8, 0});
  VERIFY( ranges::size(z3) == 3 );
  VERIFY( ranges::equal(z3, (int[]){3, 6, 9}) );

  auto z4 = views::zip_transform([] () { return 1; });
  VERIFY( ranges::size(z4) == 0 );
  static_assert( std::same_as<ranges::range_value_t<decltype(z4)>, int> );

  return true;
}

constexpr bool
test02()
{
  using __gnu_test::test_input_range;
  using __gnu_test::test_forward_range;
  using __gnu_test::test_random_access_range;

  using ty1 = ranges::zip_transform_view<std::plus<>,
					 views::all_t<test_forward_range<int>>,
					 views::all_t<test_random_access_range<int>>>;
  static_assert(ranges::forward_range<ty1>);
  static_assert(!ranges::random_access_range<ty1>);
  static_assert(!ranges::sized_range<ty1>);

  using ty2 = ranges::zip_transform_view<decltype([](int, int, int) { return 0; }),
					 views::all_t<test_forward_range<int>>,
					 views::all_t<test_input_range<int>>,
					 views::all_t<test_forward_range<int>>>;
  static_assert(ranges::input_range<ty2>);
  static_assert(!ranges::forward_range<ty2>);
  static_assert(!ranges::sized_range<ty2>);

  return true;
}

constexpr bool
test03()
{
  int u[] = {1, 2, 3, 4}, v[] = {4, 5, 6};
  auto z = views::zip_transform(std::plus{},
				u | views::filter([](auto) { return true; }),
				v);
  using ty = decltype(z);
  static_assert(ranges::forward_range<ty>);
  static_assert(!ranges::common_range<ty>);
  static_assert(!ranges::sized_range<ty>);
  VERIFY( z.begin() == z.begin() );
  VERIFY( z.begin() != z.end() );
  VERIFY( ranges::next(z.begin(), 3) == z.end() );
  auto it = z.begin();
  ++it;
  it++;
  it--;
  --it;
  VERIFY( it == z.begin() );

  return true;
}

void
test04()
{
  extern int x[5];
  struct move_only {
    move_only() { }
    move_only(move_only&&) { }
    int operator()(int i, int j) const { return i + j; }
  };
  // P2494R2 Relaxing range adaptors to allow for move only types
  static_assert( requires { views::zip_transform(move_only{}, x, x); } );
}

int
main()
{
  static_assert(test01());
  static_assert(test02());
  static_assert(test03());
  test04();
}
