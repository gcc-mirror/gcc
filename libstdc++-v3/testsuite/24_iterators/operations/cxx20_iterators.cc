// { dg-do run { target c++20 } }

#include <ranges>
#include <testsuite_iterators.h>
#include <testsuite_hooks.h>

// Bug 102181 std::advance and std::views::iota<std::int64_t> don't work
void
test_pr102181()
{
#ifdef __SIZEOF_INT128__
  using type = unsigned __int128;
#else
  using type = unsigned long;
#endif
  auto v = std::ranges::iota_view(type(0), type(10));
  auto b = v.begin();
  VERIFY( std::distance(b, std::next(b)) == 1 );
  std::advance(b, std::iter_difference_t<decltype(b)>(1));
  VERIFY( *b == 1 );
  VERIFY( std::distance(b, v.end()) == 9 );
}

// https://stackoverflow.com/questions/68100775/rangesviewtransform-produces-an-inputiterator-preventing-the-use-of-stdpre
void
test_transform_view_iterator()
{
  int a[] = {0, 1, 2, 3};
  __gnu_test::random_access_container<int> rr(a);
  auto rx = std::ranges::views::transform(rr, std::identity{});
  auto re = rx.end();
  VERIFY( *std::prev(re) == 3 );
  VERIFY( std::distance(rx.begin(), re) == 4 );

  __gnu_test::bidirectional_container<int> br(a);
  auto bx = std::ranges::views::transform(br, std::identity{});
  auto be = bx.end();
  VERIFY( *std::prev(be) == 3 );
  VERIFY( std::distance(bx.begin(), be) == 4 );

  __gnu_test::forward_container<int> fr(a);
  auto fx = std::ranges::views::transform(br, std::identity{});
  auto fb = fx.begin();
  VERIFY( *std::next(fb) == 1 );
  VERIFY( std::distance(fb, fx.end()) == 4 );

  __gnu_test::test_input_range<int> ir(a);
  auto ix = std::ranges::views::transform(ir, std::identity{});
  auto ii = ix.begin();
  std::advance(ii, 1);
  VERIFY( *ii == 1 );
  // N.B. cannot use std::distance or std::next here because there is no
  // iterator_traits<decltype(ii)>::difference_type for this iterator.
}

int main()
{
  test_pr102181();
  test_transform_view_iterator();
}
