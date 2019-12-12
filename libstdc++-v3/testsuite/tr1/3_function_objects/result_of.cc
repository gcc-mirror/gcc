// 2005-01-26 Douglas Gregor <doug.gregor -at- gmail.com>
//
// Copyright (C) 2005-2019 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-additional-options "-Wno-volatile" { target c++2a } }

// 3.4 function return types
#include <tr1/functional>
#include <tr1/type_traits>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

class X {};

struct int_result_type { typedef int result_type; };

struct int_result_of
{
  template<typename F> struct result { typedef int type; };
};

struct int_result_type_and_float_result_of
{
  typedef int result_type;
  template<typename F> struct result { typedef float type; };
};

void test01()
{
  using std::tr1::result_of;
  using std::tr1::is_same;
  using namespace __gnu_test;

  typedef int (*func_ptr)(float, double);
  typedef int (&func_ref)(float, double);
  typedef int (::X::*mem_func_ptr)(float);
  typedef int (::X::*mem_func_ptr_c)(float) const;
  typedef int (::X::*mem_func_ptr_v)(float) volatile;
  typedef int (::X::*mem_func_ptr_cv)(float) const volatile;

  VERIFY((is_same<result_of<int_result_type(float)>::type, int>::value));
  VERIFY((is_same<result_of<int_result_of(double)>::type, int>::value));
  VERIFY((is_same<result_of<int_result_of(void)>::type, void>::value));
  VERIFY((is_same<result_of<const int_result_of(double)>::type, int>::value));
  VERIFY((is_same<result_of<volatile int_result_of(void)>::type, void>::value));
  VERIFY((is_same<result_of<int_result_type_and_float_result_of(char)>::type, int>::value));
  VERIFY((is_same<result_of<func_ptr(char, float)>::type, int>::value));
  VERIFY((is_same<result_of<func_ref(char, float)>::type, int>::value));
  VERIFY((is_same<result_of<mem_func_ptr(::X,char)>::type, int>::value));
  VERIFY((is_same<result_of<mem_func_ptr_c(::X,char)>::type, int>::value));
  VERIFY((is_same<result_of<mem_func_ptr_v(::X,char)>::type, int>::value));
  VERIFY((is_same<result_of<mem_func_ptr_cv(::X,char)>::type, int>::value));
}

int main()
{
  test01();
  return 0;
}
