// { dg-options "-std=gnu++0x" }
// Copyright (C) 2008 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 20.6.4 function object return types [func.ret]
#include <functional>
#include <testsuite_hooks.h>

struct nested_result_type
{
    typedef float result_type;
    int operator()();
};

struct nested_result_template
{
  template<typename F> struct result { typedef float type; };
  int operator()(int);
};

struct cv_overload
{
  int operator()(int);
  char operator()(char) const;
  float operator()(float) volatile;
};

struct default_args
{
    int operator()(int* = 0, int* = 0);
    void operator()(void*);
};

class X {};

void test01()
{
  bool test __attribute__((unused)) = true;

  using std::result_of;
  using std::is_same;

  typedef int (*func_ptr)(float, double);
  typedef int (&func_ref)(float, double);

  VERIFY((is_same<result_of<nested_result_type()>::type, int>::value));
  VERIFY((is_same<result_of<nested_result_template(int)>::type, int>::value));
  VERIFY((is_same<result_of<cv_overload(int)>::type, int>::value));
  VERIFY((is_same<result_of<const cv_overload(int)>::type, char>::value));
  VERIFY((is_same<result_of<volatile cv_overload(int)>::type, float>::value));
  VERIFY((is_same<result_of<default_args(int*)>::type, int>::value));
  VERIFY((is_same<result_of<default_args(char*)>::type, void>::value));
  VERIFY((is_same<result_of<func_ptr(char, float)>::type, int>::value));
  VERIFY((is_same<result_of<func_ref(char, float)>::type, int>::value));
}

int main()
{
  test01();
  return 0;
}
