// 2011-03-16 Benjamin Kosnik  <bkoz@redhat.com>

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

#include <string>
#include <stdexcept>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;
  std::string s("error");

  try
    {
      logic_error e1(s);
      throw e1;
    }
  catch(const exception& e)
    {
      s = e.what();
    }

  try
    {
      domain_error e2(s);
      throw e2;
    }
  catch(const exception& e)
    {
      s = e.what();
    }

  try
    {
      invalid_argument e3(s);
      throw e3;
    }
  catch(const exception& e)
    {
      s = e.what();
    }

  try
    {
      length_error e4(s);
      throw e4;
    }
  catch(const exception& e)
    {
      s = e.what();
    }

  try
    {
      out_of_range e5(s);
      throw e5;
    }
  catch(const exception& e)
    {
      s = e.what();
    }

  try
    {
      runtime_error e6(s);
      throw e6;
    }
  catch(const exception& e)
    {
      s = e.what();
    }

  try
    {
      range_error e7(s);
      throw e7;
    }
  catch(const exception& e)
    {
      s = e.what();
    }

  try
    {
      overflow_error e8(s);
      throw e8;
    }
  catch(const exception& e)
    {
      s = e.what();
    }

  try
    {
      underflow_error e9(s);
      throw e9;
    }
  catch(const exception& e)
    {
      s = e.what();
    }
}

template<typename _Tp>
struct extra_error : public _Tp
{
  extra_error(const std::string& s) : _Tp(s) { }
};

void test02()
{
  using namespace std;
  std::string s("error");

  try
    {
      extra_error<logic_error> e1(s);
      throw e1;
    }
  catch(const exception& e)
    {
      s = e.what();
    }

  try
    {
      extra_error<domain_error> e2(s);
      throw e2;
    }
  catch(const exception& e)
    {
      s = e.what();
    }

  try
    {
      extra_error<invalid_argument> e3(s);
      throw e3;
    }
  catch(const exception& e)
    {
      s = e.what();
    }

  try
    {
      extra_error<length_error> e4(s);
      throw e4;
    }
  catch(const exception& e)
    {
      s = e.what();
    }

  try
    {
      extra_error<out_of_range> e5(s);
      throw e5;
    }
  catch(const exception& e)
    {
      s = e.what();
    }

  try
    {
      extra_error<runtime_error> e6(s);
      throw e6;
    }
  catch(const exception& e)
    {
      s = e.what();
    }

  try
    {
      extra_error<range_error> e7(s);
      throw e7;
    }
  catch(const exception& e)
    {
      s = e.what();
    }

  try
    {
      extra_error<overflow_error> e8(s);
      throw e8;
    }
  catch(const exception& e)
    {
      s = e.what();
    }

  try
    {
      extra_error<underflow_error> e9(s);
      throw e9;
    }
  catch(const exception& e)
    {
      s = e.what();
    }
}

void test03()
{
  std::logic_error le1("");
  // Copy constructor:
  std::logic_error le2(le1);
  // Copy assignment operator:
  le1 = le2;
#if __cplusplus >= 201103L
  // Move constructor:
  std::logic_error le3 = std::move(le1);
  // Move assignment operator:
  le1 = std::move(le3);
#endif

  std::runtime_error re1("");
  // Copy constructor:
  std::runtime_error re2(re1);
  // Copy assignment operator:
  re1 = re2;
#if __cplusplus >= 201103L
  // Move constructor:
  std::runtime_error re3 = std::move(re1);
  // Move assignment operator:
  re1 = std::move(re3);
#endif
}

int main(void)
{
  test01();
  test02();
  test03();
  return 0;
}
