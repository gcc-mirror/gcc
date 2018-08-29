// { dg-do run { target c++11 } }

// Copyright (C) 2009-2018 Free Software Foundation, Inc.
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

struct not_derived { virtual ~not_derived() noexcept; };
inline not_derived::~not_derived() noexcept = default;

struct uninheritable final { };

void test01() 
{
  bool test = false;

  try
  {
    std::throw_with_nested(derived());
  }
  catch (const std::nested_exception& e)
  {
    VERIFY( e.nested_ptr() == 0 );
    try
    {
      throw;
    }
    catch (const derived&)
    {
      test = true;
    }
  }
  VERIFY( test );
}

void test02() 
{
  bool test = false;

  try
  {
    std::throw_with_nested(not_derived());
  }
  catch (const std::nested_exception& e)
  {
    VERIFY( e.nested_ptr() == 0 );
    try
    {
      throw;
    }
    catch (const not_derived&)
    {
      test = true;
    }
  }
  VERIFY( test );
}

void test03()
{
  bool test = false;

  try
  {
    std::throw_with_nested(uninheritable());
  }
  catch (const std::nested_exception&)
  {
    VERIFY( false );
  }
  catch(const uninheritable&)
  {
    test = true;
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
