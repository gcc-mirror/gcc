// { dg-do run { target c++11 } }

// Copyright (C) 2011-2023 Free Software Foundation, Inc.
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


// NOTE: This makes use of the fact that we know how moveable
// is implemented on set (via swap). If the implementation changed
// this test may begin to fail.

#include <unordered_set>
#include <utility>
#include <testsuite_hooks.h>

int main()
{
  const int nb = 10000;
  std::unordered_multiset<int> ref;
  for (int i = 0; i != nb; ++i)
    {
      ref.insert(i);
      ref.insert(i);
    }

  std::unordered_multiset<int> copy(ref);
  VERIFY( copy.size() == ref.size() );
  VERIFY( std::equal(ref.begin(), ref.end(), copy.begin()) );
  return 0;
}
