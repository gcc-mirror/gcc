// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }

#include <algorithm>
#include <functional>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;

namespace ranges = std::ranges;

struct X { int i; };

void
test01()
{
    {
      int x[6] = { {2}, {4}, {6}, {8}, {10}, {11} };
      int y[6] = { {3}, {5}, {7}, {9}, {11}, {12} };
      auto [in, out] = ranges::transform(x, x, [] (int a) { return a+1; });
      VERIFY( in == x+6 && out == x+6 );
      VERIFY( ranges::equal(x, y) );
    }

    {
      X x[6] = { {2}, {4}, {6}, {8}, {10}, {11} };
      int y[7] = { {3}, {5}, {7}, {9}, {11}, {12}, {0} };
      int z[7] = { {0}, {0}, {0}, {0}, {0}, {0}, {0} };
      test_container<X, forward_iterator_wrapper> cx(x);
      test_container<int, forward_iterator_wrapper> cy(y), cz(z);
      auto [in, out]
	= ranges::transform(cx, cz.begin(), [] (int a) { return a+1; }, &X::i);
      VERIFY( ranges::equal(cy, cz) );
      VERIFY( in == cx.end() && ++out == cz.end() );
    }

    {
      X x[6] = { {2}, {4}, {6}, {8}, {10}, {11} };
      X y[7] = { {3}, {5}, {7}, {9}, {11}, {12}, {0} };
      int z[7] = { {0}, {0}, {0}, {0}, {0}, {0}, {0} };
      test_range<X, input_iterator_wrapper> rx(x), ry(y);
      test_range<int, output_iterator_wrapper> rz(z);
      auto [in, out]
	= ranges::transform(rx, rz.begin(), [] (int a) { return a+1; }, &X::i);
      VERIFY( ranges::equal(ry, z, {}, &X::i) );
      VERIFY( in == rx.end() && out.ptr == z+6 );
    }
}

struct Y { int i; int j; };

constexpr bool
test02()
{
  int x[] = { 1, 2, 3 };
  Y y[] = { {1,2}, {2,4}, {3,6} };
  ranges::transform(y, y+3, x, [] (int a) { return -a; }, &Y::i);
  return x[0] == -1 && x[1] == -2 && x[2] == -3;
}

void
test03()
{
    {
      int x[6] = { {2}, {4}, {6}, {8}, {10}, {11} };
      const int y[6] = { {3}, {5}, {7}, {9}, {11}, {12} };
      int z[6] = { {5}, {9}, {13}, {17}, {21}, {23} };
      auto [in1, in2, out] = ranges::transform(x, y, x, std::plus<>{});
      VERIFY( in1 == x+6 && in2 == y+6 && out == x+6 );
      VERIFY( ranges::equal(x, z) );
    }

    {
      int x[6] = { {2}, {4}, {6}, {8}, {10}, {11} };
      const int y[6] = { {3}, {5}, {7}, {9}, {11}, {12} };
      int z[6] = { {5}, {9}, {13}, {17}, {21}, {23} };
      auto [in1, in2, out] = ranges::transform(y, x, x, std::plus<>{});
      VERIFY( in1 == y+6 && in2 == x+6 && out == x+6 );
      VERIFY( ranges::equal(x, z) );
    }

    {
      X x[6] = { {2}, {4}, {6}, {8}, {10}, {11} };
      int y[7] = { {3}, {5}, {7}, {9}, {11}, {12}, {0} };
      int z[6] = { {5}, {9}, {13}, {17}, {21}, {23} };
      int w[6];
      test_container<X, forward_iterator_wrapper> cx(x);
      test_container<int, forward_iterator_wrapper> cy(y), cz(z), cw(w);
      auto [in1, in2, out]
	= ranges::transform(cx, cy, cw.begin(), std::plus<>{}, &X::i);
      VERIFY( in1 == cx.end() && ++in2 == cy.end() && out == cw.end() );
      VERIFY( ranges::equal(cw, cz) );
    }

    {
      X x[6] = { {2}, {4}, {6}, {8}, {10}, {11} };
      int y[7] = { {3}, {5}, {7}, {9}, {11}, {12}, {0} };
      int z[6] = { {5}, {9}, {13}, {17}, {21}, {23} };
      int w[6];
      test_range<X, input_iterator_wrapper> rx(x);
      test_range<int, input_iterator_wrapper> ry(y), rz(z);
      test_range<int, output_iterator_wrapper> rw(w);
      auto [in1, in2, out]
	= ranges::transform(rx, ry, rw.begin(), std::plus<>{}, &X::i);
      VERIFY( in1 == rx.end() && ++in2 == ry.end() && out.ptr == w+6 );
      VERIFY( ranges::equal(w, rz) );
    }
}

constexpr bool
test04()
{
  int x[3];
  const Y y[3] = { {1,2}, {2,4}, {3,6} };
  ranges::transform(y, y+3, y, y+3, x, std::plus<>{}, &Y::i, &Y::j);
  return x[0] == 3 && x[1] == 6 && x[2] == 9;
}

int
main()
{
  test01();
  static_assert(test02());
  test03();
  static_assert(test04());
}

