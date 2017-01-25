// 2005-02-27 Douglas Gregor <doug.gregor -at- gmail.com>
//
// Copyright (C) 2005-2017 Free Software Foundation, Inc.
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

// 2.1 reference wrappers
#include <tr1/functional>
#include <tr1/type_traits>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

using namespace __gnu_test;

struct X {};

struct int_result_type { typedef int result_type; };

struct derives_unary : std::unary_function<int, int> {};

struct derives_binary : std::binary_function<int, float, int> {};

struct derives_unary_binary
  : std::unary_function<int, int>,
    std::binary_function<int, float, int>
{
  typedef int result_type;
};

void test01()
{
  using std::tr1::reference_wrapper;
  using std::tr1::is_same;
  using std::tr1::is_convertible;
  using std::unary_function;
  using std::binary_function;

  // Check result_type typedef
  VERIFY((is_same<reference_wrapper<int_result_type>::result_type, int>::value));
  VERIFY((is_same<reference_wrapper<derives_unary>::result_type, int>::value));
  VERIFY((is_same<reference_wrapper<derives_binary>::result_type, int>::value));
  VERIFY((is_same<reference_wrapper<derives_unary_binary>::result_type, int>::value));
  VERIFY((is_same<reference_wrapper<int(void)>::result_type, int>::value));
  VERIFY((is_same<reference_wrapper<int(*)(void)>::result_type, int>::value));
  VERIFY((is_same<reference_wrapper<int (::X::*)()>::result_type, int>::value));
  VERIFY((is_same<reference_wrapper<int (::X::*)(float)>::result_type, int>::value));

  // Check derivation from unary_function
  VERIFY((is_convertible<reference_wrapper<derives_unary>*, unary_function<int, int>*>::value));
  VERIFY((is_convertible<reference_wrapper<derives_unary_binary>*, unary_function<int, int>*>::value));
  VERIFY((is_convertible<reference_wrapper<int(int)>*, unary_function<int, int>*>::value));
  VERIFY((is_convertible<reference_wrapper<int(*)(int)>*, unary_function<int, int>*>::value));
  VERIFY((is_convertible<reference_wrapper<int (::X::*)()>*, unary_function< ::X*, int>*>::value));
  VERIFY((is_convertible<reference_wrapper<int (::X::*)() const>*, unary_function<const ::X*, int>*>::value));
  VERIFY((is_convertible<reference_wrapper<int (::X::*)() volatile>*, unary_function<volatile ::X*, int>*>::value));
  VERIFY((is_convertible<reference_wrapper<int (::X::*)() const volatile>*, unary_function<const volatile ::X*, int>*>::value));

  // Check derivation from binary_function
  VERIFY((is_convertible<reference_wrapper<derives_binary>*, binary_function<int, float, int>*>::value));
  VERIFY((is_convertible<reference_wrapper<derives_unary_binary>*, binary_function<int, float, int>*>::value));
  VERIFY((is_convertible<reference_wrapper<int(int, float)>*, binary_function<int, float, int>*>::value));
  VERIFY((is_convertible<reference_wrapper<int(*)(int, float)>*, binary_function<int, float, int>*>::value));
  VERIFY((is_convertible<reference_wrapper<int (::X::*)(float)>*, binary_function< ::X*, float, int>*>::value));
  VERIFY((is_convertible<reference_wrapper<int (::X::*)(float) const>*, binary_function<const ::X*, float, int>*>::value));
  VERIFY((is_convertible<reference_wrapper<int (::X::*)(float) volatile>*, binary_function<volatile ::X*, float, int>*>::value));
  VERIFY((is_convertible<reference_wrapper<int (::X::*)(float) const volatile>*, binary_function<const volatile ::X*, float, int>*>::value));
}

int main()
{
  test01();
  return 0;
}
