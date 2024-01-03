// { dg-do run { target c++11 } }

// Copyright (C) 2008-2024 Free Software Foundation, Inc.
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

// C++11 23.3.4.6 Operations [forwardlist.ops]

#include <forward_list>
#include <testsuite_hooks.h>

//  Comparison functor.
template<typename Num>
  class Comp
  {
  public:
    Comp(const Num & num)
      {
        n = num;
      }
    bool operator()(const Num i, const Num j)
      {
        return (n * i) < (n * j);
      }
  private:
    Num n;
  };

// This test verifies the following:
//   sort
void
test01()
{
  const unsigned int n = 13;
  int order[][n] = {
    { 0,1,2,3,4,5,6,7,8,9,10,11,12 },
    { 6,2,8,4,11,1,12,7,3,9,5,0,10 },
    { 12,11,10,9,8,7,6,5,4,3,2,1,0 },
  };
  std::forward_list<int> sorted(order[0], order[0] + n);

  for (unsigned int i = 0; i < sizeof(order)/sizeof(*order); ++i)
    {
      std::forward_list<int> head(order[i], order[i] + n);

      head.sort();

      VERIFY(head == sorted);
    }

  std::forward_list<int> reversed(order[2], order[2] + n);
  for (unsigned int i = 0; i < sizeof(order)/sizeof(*order); ++i)
    {
      std::forward_list<int> head(order[i], order[i] + n);

      Comp<int> comp(-1);
      head.sort( comp );

      VERIFY(head == reversed);
    }
}

int
main()
{
  test01();
  return 0;
}
