// Copyright (C) 2017-2025 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_hooks.h>

class P1
{
public:
  using element_type = int;

  explicit P1(int* p)
  : p_(p) { }

  int* operator->() const noexcept
  { return p_; }

private:
  int* p_;
};

class P2
{
public:
  using element_type = int;

  explicit P2(int* p)
  : p_(p) { }

  P1 operator->() const noexcept
  { return p_; }

private:
  P1 p_;
};

class P3
{
public:
  explicit P3(int* p)
  : p_(p) { }

  int* get() const noexcept
  { return p_; }

private:
  int* p_;
};

namespace std
{
  template<>
    struct pointer_traits<::P3>
    {
      static int* to_address(const ::P3& p) noexcept
      { return p.get(); }
    };
}

class P4
{
public:
  explicit P4(int* p)
  : p_(p) { }

  int* operator->() const noexcept
  { return nullptr; }

  int* get() const noexcept
  { return p_; }

private:
  int* p_;
};

namespace std
{
  template<>
    struct pointer_traits<::P4>
    {
      static int* to_address(const ::P4& p) noexcept
      { return p.get(); }
    };
}

void test01()
{
  int i = 0;
  int* p = &i;
  VERIFY( std::to_address(p) == &i );
}

void test02()
{
  int i = 0;
  P1 p(&i);
  VERIFY( std::to_address(p) == &i );
}

void test03()
{
  int i = 0;
  P2 p(&i);
  VERIFY( std::to_address(p) == &i );
}

void test04()
{
  int i = 0;
  P3 p(&i);
  VERIFY( std::to_address(p) == &i );
}

void test05()
{
  int i = 0;
  P4 p(&i);
  VERIFY( std::to_address(p) == &i );
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  return 0;
}
