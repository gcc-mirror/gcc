// { dg-options "-std=gnu++0x" }
// { dg-require-atomic-builtins "" }

// Copyright (C) 2009-2014 Free Software Foundation, Inc.
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

#include <exception>
#include <testsuite_hooks.h>

struct derived : std::nested_exception { };

struct base { virtual ~base() noexcept; };
inline base::~base() noexcept = default;

struct derived2 : base, std::nested_exception { };

void test01() 
{
  bool test __attribute__((unused)) = false;

  try
  {
    throw 42;
  }
  catch (...)
  {
    derived d;
    try
    {
      std::rethrow_if_nested(d);
    }
    catch (const int& i)
    {
      test = true;
      VERIFY( i == 42 );
    }
  }

  VERIFY( test );
}

void test02() 
{
  bool test __attribute__((unused)) = false;

  try
  {
    throw base();
  }
  catch (const base& b)
  {
    std::rethrow_if_nested(b);
    test = true;
  }

  VERIFY( test );
}

void test03() 
{
  bool test __attribute__((unused)) = false;

  try
  {
    throw 42;
  }
  catch (...)
  {
    try
    {
      throw derived2();
    }
    catch (const base& b)
    {
      try
      {
        std::rethrow_if_nested(b);
      }
      catch (const int& i)
      {
        VERIFY( i == 42 );
        test = true;
      }
    }
  }

  VERIFY( test );
}


int main()
{
  test01();
  test02();
  test03();
  return 0;
}
