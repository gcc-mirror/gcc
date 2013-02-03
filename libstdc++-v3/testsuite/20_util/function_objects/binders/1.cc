// Copyright (C) 2005-2013 Free Software Foundation, Inc.
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

// 20.3.6 Binders

// { dg-do compile }

#include <functional>
using namespace std;

struct s
{
  void f_void_int_const(int) const {}
  void f_void_int(int) {}
  int f_int_int_const(int) const { return 1; }
  int f_int_int(int) {return 1; }
  void f_void_void_const() const {}
  void f_void_void() {}
  int f_int_void_const() const { return 1; }
  int f_int_void() { return 1; }
};

void test01(s& a)
{ 
  mem_fun_t<void, s> p1(&s::f_void_void);
  mem_fun_t<int, s> p2(&s::f_int_void);
  p1(&a);
  p2(&a);
  mem_fun1_t<void, s, int> q1(&s::f_void_int);
  mem_fun1_t<int, s, int> q2(&s::f_int_int);
  q1(&a,0);
  q2(&a,0);

  (mem_fun(&s::f_void_void))(&a);
  (mem_fun(&s::f_void_int))(&a,0);
  (mem_fun(&s::f_int_void))(&a);
  (mem_fun(&s::f_int_int))(&a,0);

  mem_fun_ref_t<void, s> ref1(&s::f_void_void);
  mem_fun_ref_t<int, s> ref2(&s::f_int_void);

  ref1(a);
  ref2(a);

  mem_fun1_ref_t<void, s, int> ref3(&s::f_void_int);
  mem_fun1_ref_t<int, s, int> ref4(&s::f_int_int); 

  ref3(a,0);
  ref4(a,0);

  (mem_fun_ref(&s::f_void_void))(a);
  (mem_fun_ref(&s::f_void_int))(a, 0);
  (mem_fun_ref(&s::f_int_void))(a);
  (mem_fun_ref(&s::f_int_int))(a, 0);
}

void test02(const s& a)
{
  const_mem_fun_t<void, s> p1(&s::f_void_void_const);
  const_mem_fun_t<int, s> p2(&s::f_int_void_const);
  p1(&a);
  p2(&a);
  const_mem_fun1_t<void, s, int> q1(&s::f_void_int_const);
  const_mem_fun1_t<int, s, int> q2(&s::f_int_int_const);
  q1(&a,0);
  q2(&a,0);

  (mem_fun(&s::f_void_void_const))(&a);
  (mem_fun(&s::f_void_int_const))(&a, 0);
  (mem_fun(&s::f_int_void_const))(&a);
  (mem_fun(&s::f_int_int_const))(&a, 0);

  const_mem_fun_ref_t<void, s> ref1(&s::f_void_void_const);
  const_mem_fun_ref_t<int, s> ref2(&s::f_int_void_const);

  ref1(a);
  ref2(a);

  const_mem_fun1_ref_t<void, s, int> ref3(&s::f_void_int_const);
  const_mem_fun1_ref_t<int, s, int> ref4(&s::f_int_int_const); 

  ref3(a,0);
  ref4(a,0);

  (mem_fun_ref(&s::f_void_void_const))(a);
  (mem_fun_ref(&s::f_void_int_const))(a, 0);
  (mem_fun_ref(&s::f_int_void_const))(a);
  (mem_fun_ref(&s::f_int_int_const))(a, 0);
}
