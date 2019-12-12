// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

// Copyright (C) 2009-2019 Free Software Foundation, Inc.
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

// Test that promise::set_exception throws when required.

#include <future>
#include <testsuite_hooks.h>

// Check for promise_already_satisfied error conditions.

void test01()
{
  bool test = false;

  std::promise<int> p1;
  std::future<int> f1 = p1.get_future();

  p1.set_exception(std::make_exception_ptr(0));

  try
  {
    p1.set_exception(std::make_exception_ptr(1));
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY(e.code() ==
        std::make_error_code(std::future_errc::promise_already_satisfied));
    test = true;
  }

  try
  {
    f1.get();
    test = false;
  }
  catch(int i)
  {
    VERIFY( i == 0 );
  }

  VERIFY( test );
}

void test02()
{
  bool test = false;

  std::promise<int> p1;
  std::future<int> f1 = p1.get_future();

  p1.set_value(2);

  try
  {
    p1.set_exception(std::make_exception_ptr(0));
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY(e.code() ==
        std::make_error_code(std::future_errc::promise_already_satisfied));
    test = true;
  }

  VERIFY( test );
}

void test03()
{
  bool test = false;

  std::promise<int&> p1;
  std::future<int&> f1 = p1.get_future();

  p1.set_exception(std::make_exception_ptr(0));

  try
  {
    p1.set_exception(std::make_exception_ptr(1));
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY(e.code() ==
        std::make_error_code(std::future_errc::promise_already_satisfied));
    test = true;
  }

  try
  {
    f1.get();
    test = false;
  }
  catch(int i)
  {
    VERIFY( i == 0 );
  }

  VERIFY( test );
}

void test04()
{
  bool test = false;

  std::promise<int&> p1;
  std::future<int&> f1 = p1.get_future();

  int i = 2;
  p1.set_value(i);

  try
  {
    p1.set_exception(std::make_exception_ptr(0));
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY(e.code() ==
        std::make_error_code(std::future_errc::promise_already_satisfied));
    test = true;
  }

  VERIFY( test );
}

void test05()
{
  bool test = false;

  std::promise<void> p1;
  std::future<void> f1 = p1.get_future();

  p1.set_exception(std::make_exception_ptr(0));

  try
  {
    p1.set_exception(std::make_exception_ptr(1));
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY(e.code() ==
        std::make_error_code(std::future_errc::promise_already_satisfied));
    test = true;
  }

  try
  {
    f1.get();
    test = false;
  }
  catch(int i)
  {
    VERIFY( i == 0 );
  }

  VERIFY( test );
}

void test06()
{
  bool test = false;

  std::promise<void> p1;
  std::future<void> f1 = p1.get_future();

  p1.set_value();

  try
  {
    p1.set_exception(std::make_exception_ptr(0));
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY(e.code() ==
        std::make_error_code(std::future_errc::promise_already_satisfied));
    test = true;
  }

  VERIFY( test );
}

// Check for no_state error condition (PR libstdc++/80316)

void test07()
{
  using namespace std;

  promise<int> p1;
  promise<int> p2(std::move(p1));
  try
  {
    p1.set_exception(std::make_exception_ptr(1));
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY(e.code() == make_error_code(future_errc::no_state));
  }
}

void test08()
{
  using namespace std;

  promise<int&> p1;
  promise<int&> p2(std::move(p1));
  try
  {
    int i = 0;
    p1.set_exception(std::make_exception_ptr(1));
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY(e.code() == make_error_code(future_errc::no_state));
  }
}

void test09()
{
  using namespace std;

  promise<void> p1;
  promise<void> p2(std::move(p1));
  try
  {
    p1.set_exception(std::make_exception_ptr(1));
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY(e.code() == make_error_code(future_errc::no_state));
  }
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
  test08();
  test09();
  return 0;
}
