// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

// Copyright (C) 2010-2018 Free Software Foundation, Inc.
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

// 30.6.6 Class template future [futures.unique_future]

#include <future>
#include <testsuite_hooks.h>

// This test verifies behaviour which is encouraged by a non-normative note,
// but not required.
 
void
test01()
{
  std::promise<int> p;
  std::future<int> f = p.get_future();
  p.set_value(0);
  f.get();
  try
  {
    f.get();
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY( e.code() == std::future_errc::no_state );
  }
}

void
test02()
{
  std::promise<int&> p;
  std::future<int&> f = p.get_future();
  int i = 0;
  p.set_value(i);
  f.get();
  try
  {
    f.get();
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY( e.code() == std::future_errc::no_state );
  }
}

void
test03()
{
  std::promise<void> p;
  std::future<void> f = p.get_future();
  p.set_value();
  f.get();
  try
  {
    f.get();
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY( e.code() == std::future_errc::no_state );
  }
}

int main()
{
  test01();
  test02();
  test03();

  return 0;
}

