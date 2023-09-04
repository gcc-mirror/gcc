// { dg-do run { target c++20 } }
// { dg-require-effective-target hosted }

#include <algorithm>
#include <ranges>
#include <string_view>
#include <testsuite_hooks.h>

// Verify P1739R4 (and LWG 3407) changes to views::take/drop/counted.

namespace ranges = std::ranges;
namespace views = ranges::views;

void
test01()
{
  ranges::empty_view<int> v;
  using ty = decltype(v);
  using ty = decltype(views::take(v, 42));
  using ty = decltype(views::drop(v, 42));
}

void
test02()
{
  int x[] = {1,2,3,4,5};
  std::span r(x, x+5);
  using ty = decltype(r);

  std::same_as<ty> auto v1 = views::take(r, 2);
  VERIFY( ranges::equal(v1, (int[]){1,2}) );

  std::same_as<ty> auto v2 = views::drop(r, 3);
  VERIFY( ranges::equal(v2, (int[]){4,5}) );

  std::same_as<ty> auto v3 = views::counted(r.begin(), 4);
  VERIFY( ranges::equal(v3, (int[]){1,2,3,4}) );
}

void
test03()
{
  using namespace std::literals;
  std::string_view r = "hello world"sv;
  using ty = decltype(r);

  std::same_as<ty> auto v1 = views::take(r, 5);
  VERIFY( ranges::equal(v1, "hello"sv) );

  std::same_as<ty> auto v2 = views::drop(r, 6);
  VERIFY( ranges::equal(v2, "world"sv) );
}

void
test04()
{
  int x[] = {1,2,3,4,5};
  ranges::subrange r(x, x+5);
  using ty = decltype(r);

  std::same_as<ty> auto v1 = views::take(r, 2);
  VERIFY( ranges::equal(v1, (int[]){1,2}) );

  std::same_as<ty> auto v2 = views::drop(r, 1);
  VERIFY( ranges::equal(v2, (int[]){2,3,4,5}) );
}

void
test05()
{
  auto r1 = views::iota(1, 6);
  using ty1 = decltype(r1);

  std::same_as<ty1> auto v1 = views::take(r1, 2);
  VERIFY( ranges::equal(v1, (int[]){1,2}) );

  std::same_as<ty1> auto v2 = views::drop(r1, 3);
  VERIFY( ranges::equal(v2, (int[]){4,5}) );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
}
