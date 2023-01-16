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

#include <memory>
#include <testsuite_hooks.h>

struct Deleter
{
  Deleter() = default;
  Deleter(const Deleter&) = default;
  Deleter(Deleter&&) = default;
  
  Deleter&
  operator=(const Deleter&)
  {
    VERIFY( true );
    return *this;
  }

  Deleter&
  operator=(Deleter&&)
  {
    VERIFY( false );
    return *this;
  }

  template<class T>
    void
    operator()(T*) const { }
};

struct DDeleter : Deleter { };

// libstdc++/48635
void test01()
{
  Deleter d;

  std::unique_ptr<int, Deleter&> p1(nullptr, d), p2(nullptr, d);
  p2 = std::move(p1);

  DDeleter dd;

  std::unique_ptr<int[], Deleter&> p1a(nullptr, d), p2a(nullptr, d);
  p2a = std::move(p1a);
}

int main()
{
  test01();
  return 0;
}
