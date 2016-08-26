// { dg-do run { target c++11 } }

// Copyright (C) 2012-2016 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <vector>
#include <testsuite_hooks.h>

template<class Cont>
  void
  my_reverse(Cont& c)
  {
    for (std::size_t i = 0, j = c.size(); i < j; ++i)
      {
	--j;
	using std::swap;
	swap(c[i], c[j]);
      }
  }

template<class Cont>
  void
  my_compare(const Cont& c1, const Cont& c2)
  {
    bool test __attribute__((unused)) = true;

    VERIFY( c1.size() == c2.size() );

    for (std::size_t i = 0; i < c1.size(); ++i)
      VERIFY( c1[i] == c2[c1.size() - i - 1] );
  }

void test01()
{
  const std::vector<bool> vb_ref{0, 1, 1, 0, 1};
  std::vector<bool>       vb(vb_ref);
  my_reverse(vb);
  my_compare(vb_ref, vb);
}

int main()
{
  test01();
  return 0;
}
