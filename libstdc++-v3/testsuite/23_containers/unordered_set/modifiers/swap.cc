// Copyright (C) 2014-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <unordered_set>
#include <testsuite_hooks.h>

void test01()
{
  std::unordered_set<int> us1 { 0, 1 };
  {
    std::unordered_set<int> us2(std::move(us1));

    us1.swap(us2);

    VERIFY( us1.find(0) != us1.end() );

    us1.insert(2);

    VERIFY( us1.size() == 3 );

    us2.swap(us1);

    VERIFY( us2.size() == 3 );
    VERIFY( us2.find(2) != us2.end() );

    us1 = { 3, 4, 5 };

    VERIFY( us1.size() == 3 );
    VERIFY( us1.bucket_count() >= 3 );

    std::unordered_set<int> us3(std::move(us1));
    us3 = std::move(us2);

    us1.swap(us2);

    VERIFY( us1.empty() );
    VERIFY( us2.empty() );
  }

  us1 = { 0, 1 };
  VERIFY( us1.size() == 2 );
}

int main()
{
  test01();
  return 0;
}
