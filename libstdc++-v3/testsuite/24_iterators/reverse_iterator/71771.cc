// Copyright (C) 2016-2020 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <iterator>
#include <testsuite_iterators.h>

// PR libstdc++/71771

template<typename Iter>
auto
diff2(std::reverse_iterator<Iter> it1, std::reverse_iterator<Iter> it2)
-> decltype(it1 - it2)
{ return it1 - it2; }

template<typename Iter>
void
diff2(Iter, Iter)
{ }

void
test01()
{
  int i[2];
  __gnu_test::test_container<int, __gnu_test::bidirectional_iterator_wrapper>
    c(i);
  using reverse_iterator
    = std::reverse_iterator<__gnu_test::bidirectional_iterator_wrapper<int>>;
  diff2(reverse_iterator(c.end()), reverse_iterator(c.begin()));
}
