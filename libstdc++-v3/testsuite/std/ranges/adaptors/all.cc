// Copyright (C) 2020-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <algorithm>
#include <ranges>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;
namespace views = std::ranges::views;

void
test01()
{
  int x[] = {1,2,3,4,5};
  auto v = views::all(x);

  static_assert(ranges::view<decltype(v)>);
  static_assert(ranges::random_access_range<decltype(v)>);

  VERIFY( ranges::size(v) == 5 );
  VERIFY( ranges::size(x | views::all) == 5 );
  VERIFY( ranges::size(v | views::all | views::all) == 5 );
  VERIFY( ranges::size(v | (views::all | views::all)) == 5 );

  ranges::reverse(v);
  VERIFY( ranges::equal(v, (int[]){5,4,3,2,1}) );
}

void
test02()
{
  int x[5] = { 0 };
  int k = 0;
  for (auto&& i : ranges::ref_view{x})
    i += ++k;
  VERIFY( ranges::equal(x, (int[]){1,2,3,4,5}) );
}

constexpr bool
test03()
{
  std::array ints{0,1,2,3,4,5};
  auto even = [] (int i) { return i%2==0; };
  auto odd = [] (int i) { return i%2==1; };
  auto square = [] (int i) { return i*i; };
  int sum = 0;
  for (auto v : (ints
		 | (views::all
		    | (views::filter(even)
		    | (views::filter(odd) | views::all)))
		 | views::transform(square)))
    sum += v;
  return sum == 0;
}

constexpr bool
test04()
{
  auto odd = [] (int i) { return i%2==1; };
  auto square = [] (int i) { return i*i; };
  auto increment = [] (int i) { return i+1; };
  auto small = [] (int i) { return i<30; };
  auto non_negative = [] (int i) { return i>=0; };
  auto negative = [] (int i) { return i<0; };
  return ranges::equal(views::iota(-5)
		       | views::drop_while(negative)
		       | views::take_while(non_negative)
		       | views::transform(increment)
		       | views::filter(odd)
		       | views::take(3)
		       | views::all
		       | views::transform(square),
		       views::iota(-5)
		       | views::drop_while(negative)
		       | views::drop(1)
		       | views::filter(odd)
		       | views::transform(square)
		       | views::take_while(small)
		       | views::take_while(small));
}

static_assert(std::is_empty_v<decltype(views::common
				       | views::join
				       | views::all
				       | views::common
				       | views::keys
				       | views::reverse)>);
#if 0
// Adding empty range adaptor closure objects to a pipeline used to not
// increase the size of the pipeline, but now that our range adaptor closure
// objects derive from a common empty base class, [[no_unique_address]] can no
// longer make two empty adjacent range adaptor closure objects occupy the same
// data member address.
static_assert(sizeof(decltype(views::take(5) | views::drop(5)))
	      == sizeof(decltype(views::take(5)
				 | views::join
				 | views::common
				 | views::all
				 | views::keys
				 | views::drop(5)
				 | views::reverse)));
#endif

template<auto all = views::all>
void
test05()
{
  // Verify SFINAE behavior.
  static_assert(!requires { all(); });
  static_assert(!requires { all(0, 0); });
  static_assert(!requires { all(0); });
  static_assert(!requires { 0 | all; });
}

template<bool B1, bool B2>
struct BorrowedRange
{
  int* ptr = nullptr;

  BorrowedRange(int (&arr)[3]) noexcept : ptr(arr) { }

  int* begin() const noexcept(B1) { return ptr; }
  int* end() const noexcept(B2) { return ptr + 3; }
};

template<bool B1, bool B2>
const bool std::ranges::enable_borrowed_range<BorrowedRange<B1, B2>> = true;

void
test06()
{
  int x[] { 1, 2, 3 };

  // Using ref_view:
  static_assert(noexcept(views::all(x)));

  // Using subrange:
  static_assert(noexcept(views::all(BorrowedRange<true, true>(x))));
  static_assert(!noexcept(views::all(BorrowedRange<true, false>(x))));
  static_assert(!noexcept(views::all(BorrowedRange<false, true>(x))));
  static_assert(!noexcept(views::all(BorrowedRange<false, false>(x))));
}

int
main()
{
  test01();
  test02();
  static_assert(test03());
  static_assert(test04());
  test05();
  test06();
}
