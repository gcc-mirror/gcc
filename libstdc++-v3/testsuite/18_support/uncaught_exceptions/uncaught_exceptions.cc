// Copyright (C) 2015-2023 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }

#include <cassert>
#include <exception>
#include <testsuite_hooks.h>

#ifndef __cpp_lib_uncaught_exceptions
# error "Feature-test macro for uncaught_exceptions missing"
#elif __cpp_lib_uncaught_exceptions != 201411
# error "Feature-test macro for uncaught_exceptions has wrong value"
#endif

struct UncaughtVerifier
{
  int expected_count_ = 0;
  UncaughtVerifier(int expected_count) : expected_count_(expected_count) {}
  ~UncaughtVerifier()
  {
    VERIFY(std::uncaught_exceptions() == expected_count_);
  }
};

struct Transaction
{
  int initial_count_;
  bool& result_;
  Transaction(bool& result)
  : initial_count_(std::uncaught_exceptions()),
    result_(result) {}
  ~Transaction()
  {
    if (std::uncaught_exceptions() != initial_count_) {
      result_ = false;
    }
  }
};

void test01()
{
  try {
    UncaughtVerifier uv{0};
  } catch (...) {
    UncaughtVerifier uv{0};
  }
}

void test02()
{
  try {
    UncaughtVerifier uv{1};
    throw 0;
  } catch (...) {
    UncaughtVerifier uv{0};
  }
}

void test03()
{
  try {
    struct Wrap
    {
      UncaughtVerifier uv_{1};
      ~Wrap() {try {UncaughtVerifier uv2{2}; throw 0;} catch(...) {}}
    };
    Wrap w;
    throw 0;
  } catch (...) {
    UncaughtVerifier uv{0};
  }
}

void test04()
{
  bool result = true;
  try {
    Transaction t{result};
  } catch(...) {
  }
  VERIFY(result);
}

void test05()
{
  bool result = true;
  try {
    Transaction t{result};
    throw 0;
  } catch(...) {
  }
  VERIFY(!result);
}

void test06()
{
  bool result = true;
  bool result2 = true;
  try {
    struct Wrap
    {
      bool& result_;
      Wrap(bool& result) : result_(result) {}
      ~Wrap()
      {
        Transaction t{result_};
      }
    };
    Transaction t{result};
    Wrap w{result2};
    throw 0;
  } catch(...) {
  }
  VERIFY(!result);
  VERIFY(result2);
}

void test07()
{
  bool result = true;
  bool result2 = true;
  try {
    struct Wrap
    {
      bool& result_;
      Wrap(bool& result) : result_(result) {}
      ~Wrap()
      {
        try {
          Transaction t{result_};
          throw 0;
        } catch(...) {}
      }
    };
    Transaction t{result};
    Wrap w{result2};
    throw 0;
  } catch(...) {
  }
  VERIFY(!result);
  VERIFY(!result2);
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
}
