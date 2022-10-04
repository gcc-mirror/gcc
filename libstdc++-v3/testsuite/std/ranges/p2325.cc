// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }
// { dg-require-effective-target hosted }
// P2325R3 "Views should not be required to be default constructible"

#include <ranges>
#include <iterator>
#include <span>
#include <sstream>
#include <vector>
#include <testsuite_iterators.h>

using namespace std;

template<default_initializable T> void f();
template<typename T> requires weakly_incrementable<T> || ranges::view<T> void f();

void
test01()
{
  // Verify neither std::weakly_incrementable nor ranges::view require
  // default_initializable.
  f<int>(); // { dg-error "ambiguous" }
}

void
test02()
{
  // Verify these iterators are not default constructible.
  static_assert(!default_initializable<insert_iterator<vector<int>>>);
  static_assert(!default_initializable<front_insert_iterator<vector<int>>>);
  static_assert(!default_initializable<back_insert_iterator<vector<int>>>);
  static_assert(!default_initializable<ostream_iterator<int>>);

  using iter = ostream_iterator<int>;

  // Verify common_iterator is conditionally default constructible.
  static_assert(!default_initializable<common_iterator<iter, unreachable_sentinel_t>>);
  static_assert(default_initializable<common_iterator<int*, unreachable_sentinel_t>>);

  // Verify counted_iterator is conditionally default constructible.
  static_assert(!default_initializable<counted_iterator<iter>>);
  static_assert(default_initializable<counted_iterator<int*>>);
}

void
test03()
{
  using iter = ostream_iterator<int>;

  // Verify iota_view is conditionally default constructible.
  static_assert(!default_initializable<ranges::iota_view<iter>>);
  static_assert(!default_initializable<decltype(declval<ranges::iota_view<iter>>().begin())>);
  static_assert(default_initializable<ranges::iota_view<int>>);
  static_assert(default_initializable<decltype(declval<ranges::iota_view<int>>().begin())>);

  // Verify subrange is conditionally default constructible.
  static_assert(!default_initializable<ranges::subrange<iter, unreachable_sentinel_t>>);
  static_assert(default_initializable<ranges::subrange<int*, unreachable_sentinel_t>>);

  // Verify single_view is conditionally default constructible.
  static_assert(!default_initializable<ranges::single_view<iter>>);
  static_assert(default_initializable<ranges::single_view<int*>>);
}

void
test04()
{
  // Verify basic_istream_view is not default constructible.
  using type = ranges::basic_istream_view<int, char, char_traits<char>>;
  static_assert(!default_initializable<type>);
  static_assert(!default_initializable<decltype(declval<type>().begin())>);
}

void
test05()
{
  // Verify ref_view is not default constructible.
  static_assert(!default_initializable<ranges::ref_view<int[5]>>);
}

template<auto adaptor>
void
test06()
{
  auto f1 = [] (auto) { return true; };
  auto f2 = [i=0] (auto) { return true; };
  static_assert(default_initializable<decltype(views::single(0) | adaptor(f1))>);
  static_assert(!default_initializable<decltype(views::single(0) | adaptor(f2))>);

  struct S { S() = delete; };
  static_assert(!default_initializable<decltype(views::single(declval<S>()) | adaptor(f1))>);
  static_assert(!default_initializable<decltype(views::single(declval<S>()) | adaptor(f2))>);
}

// Verify filter_view, transform_view, take_while_view and drop_while_view are
// conditionally default constructible.
template void test06<views::filter>();
template void test06<views::transform>();
template void test06<views::take_while>();
template void test06<views::drop_while>();

void
test07()
{
  // Verify join_view is conditionally default constructible.
  struct S { S() = delete; };
  using type1 = ranges::join_view<ranges::single_view<ranges::single_view<S>>>;
  static_assert(!default_initializable<type1>);
  using type2 = ranges::join_view<ranges::single_view<ranges::single_view<int>>>;
  static_assert(default_initializable<type2>);
}

void
test08()
{
  // Verify lazy_split_view is conditionally default constructible.
  using type1 = ranges::lazy_split_view<ranges::ref_view<int[2]>, ranges::single_view<int>>;
  static_assert(!default_initializable<type1>);
  using type2 = ranges::lazy_split_view<ranges::single_view<int>, ranges::ref_view<int[2]>>;
  static_assert(!default_initializable<type2>);
  using type3 = ranges::lazy_split_view<ranges::ref_view<int[2]>, ranges::ref_view<int[2]>>;
  static_assert(!default_initializable<type3>);
  using type4 = ranges::lazy_split_view<ranges::single_view<int>, ranges::single_view<int>>;
  static_assert(default_initializable<type4>);
}

void
test08a()
{
  // Verify split_view is conditionally default constructible.
  using type1 = ranges::split_view<ranges::ref_view<int[2]>, ranges::single_view<int>>;
  static_assert(!default_initializable<type1>);
  using type2 = ranges::split_view<ranges::single_view<int>, ranges::ref_view<int[2]>>;
  static_assert(!default_initializable<type2>);
  using type3 = ranges::split_view<ranges::ref_view<int[2]>, ranges::ref_view<int[2]>>;
  static_assert(!default_initializable<type3>);
  using type4 = ranges::split_view<ranges::single_view<int>, ranges::single_view<int>>;
  static_assert(default_initializable<type4>);
}

void
test09()
{
  // Verify common_view is conditionally default constructible.
  using type1 = ranges::common_view<ranges::iota_view<ostream_iterator<int>>>;
  static_assert(!default_initializable<type1>);
  using type2 = ranges::common_view<ranges::iota_view<int*>>;
  static_assert(default_initializable<type2>);
}

void
test10()
{
  // Verify reverse_view is conditionally default constructible.
  using type1 = ranges::reverse_view<ranges::ref_view<int[2]>>;
  static_assert(!default_initializable<type1>);
  using type2 = ranges::reverse_view<ranges::single_view<int>>;
  static_assert(default_initializable<type2>);
}

void
test11()
{
  // Verify elements_view is conditionally default constructible.
  using type1 = ranges::elements_view<ranges::ref_view<pair<int,int>[2]>, 0>;
  static_assert(!default_initializable<type1>);
  using type2 = ranges::elements_view<ranges::single_view<pair<int,int>>, 0>;
  static_assert(default_initializable<type2>);
}
