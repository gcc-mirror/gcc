// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

// Copyright (C) 2014-2019 Free Software Foundation, Inc.
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

// Test set_value_at_thread_exit error conditions

#include <future>
#include <testsuite_hooks.h>

void test01()
{
  std::promise<int> p1;
  p1.set_value(1);
  try
  {
    p1.set_value_at_thread_exit(2);
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY( e.code() == std::future_errc::promise_already_satisfied );
  }
  try
  {
    p1.set_exception_at_thread_exit(std::make_exception_ptr(3));
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY( e.code() == std::future_errc::promise_already_satisfied );
  }

  std::promise<int> p2(std::move(p1));
  try
  {
    p1.set_value_at_thread_exit(2);
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY( e.code() == std::future_errc::no_state );
  }
  try
  {
    p1.set_exception_at_thread_exit(std::make_exception_ptr(3));
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY( e.code() == std::future_errc::no_state );
  }
}

void test02()
{
  std::promise<int&> p1;
  int i = 1;
  p1.set_value(i);
  try
  {
    p1.set_value_at_thread_exit(i);
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY( e.code() == std::future_errc::promise_already_satisfied );
  }
  try
  {
    p1.set_exception_at_thread_exit(std::make_exception_ptr(3));
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY( e.code() == std::future_errc::promise_already_satisfied );
  }

  std::promise<int&> p2(std::move(p1));
  try
  {
    int i = 0;
    p1.set_value_at_thread_exit(i);
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY( e.code() == std::future_errc::no_state );
  }
  try
  {
    p1.set_exception_at_thread_exit(std::make_exception_ptr(3));
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY( e.code() == std::future_errc::no_state );
  }
}

void test03()
{
  std::promise<void> p1;
  int i = 0;
  p1.set_value();
  try {
    p1.set_value_at_thread_exit();
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY( e.code() == std::future_errc::promise_already_satisfied );
  }
  try
  {
    p1.set_exception_at_thread_exit(std::make_exception_ptr(3));
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY( e.code() == std::future_errc::promise_already_satisfied );
  }

  std::promise<void> p2(std::move(p1));
  try {
    p1.set_value_at_thread_exit();
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY( e.code() == std::future_errc::no_state );
  }
  try
  {
    p1.set_exception_at_thread_exit(std::make_exception_ptr(3));
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
}
