// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;

namespace ranges = std::ranges;

void
test01()
{
  int x[] = {1,2,3,4,5,6,7};

    {
      const int y[] = {2,4,6};
      int w[7];
      test_range<int, input_iterator_wrapper> rx(x);
      test_range<int, output_iterator_wrapper> rw(w);
      auto [in,out] = ranges::copy_if(rx, rw.begin(),
				      [] (int a) { return (a%2)==0; });
      VERIFY( in == rx.end() && out.ptr == w+3 );
      VERIFY( ranges::equal(w, w+3, y, y+3) );
    }

    {
      const int y[] = {1,3,5,7};
      int w[7];
      test_range<int, input_iterator_wrapper> rx(x);
      test_range<int, output_iterator_wrapper> rw(w);
      auto [in,out] = ranges::copy_if(rx, rw.begin(),
				      [] (int a) { return (a%2)==0; },
				      [] (int a) { return a+1; });
      VERIFY( in == rx.end() && out.ptr == w+4 );
      VERIFY( ranges::equal(w, w+4, y, y+4) );
    }
}

constexpr bool
test02()
{
  int x[] = {1,2,3};
  const int y[] = {1,3};
  int w[3];
  auto [in,out] = ranges::copy_if(x, w, [] (int a) { return (a%2)==1; });
  return ranges::equal(w, out, y, y+2);
}

int
main()
{
  test01();
  static_assert(test02());
}
