// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

// C++98 24.5.1 Template class istream_iterator

#include <iterator>
#include <sstream>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

void test01()
{
  using namespace std;

  const int N = 6;
  int arr[N]  = { };
  using __gnu_test::test_container;
  using __gnu_test::output_iterator_wrapper;
  test_container<int, output_iterator_wrapper> con(arr, arr+N);
  output_iterator_wrapper<int> out = con.begin();

  istringstream ss("1 2 3 4 5");
  std::istream_iterator<int> iter(ss), end;
  while (iter != end)
    *out++ = *iter++;
  VERIFY( iter == end );
  for (int i = 0; i < N; ++i)
    VERIFY( arr[i] == (i + 1) % N );

  std::istream_iterator<int> iter2(ss);
  VERIFY( iter2 == end );

  ss.clear();
  ss.str("-1 -2 -3");
  VERIFY( iter == end );

#ifndef _GLIBCXX_DEBUG
  // This is undefined, so aborts under debug mode.
  // Without debug mode, it should not extract anything from the stream,
  // and the iterator should remain at end-of-stream.
  ++iter;
  VERIFY( iter == end );
#endif

  std::istream_iterator<int> iter3(ss);
  VERIFY( iter3 != end );
  VERIFY( iter3 != iter );
  VERIFY( *iter3 == -1 );
  ++iter3;
  VERIFY( *iter3 == -2 );

  iter2 = iter3;
  VERIFY( *iter2 == -2 );
  ++iter2;
  VERIFY( *iter2 == -3 );
  ++iter2;
  VERIFY( iter2 == end );
}

int main()
{
  test01();
}
