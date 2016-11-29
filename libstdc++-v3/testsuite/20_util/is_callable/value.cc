// Copyright (C) 2016 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do compile }

#include <type_traits>

#ifndef IS_CALLABLE_DEFINED
template<typename T, typename R = void>
  constexpr bool is_callable()
  {
    static_assert(std::is_callable<T, R>::value == std::is_callable_v<T, R>);
    return std::is_callable_v<T, R>;
  }
#endif

void test01()
{
  using func_type_v0 = void(*)();

  static_assert(   is_callable< func_type_v0() >(),	    "");
  static_assert(   is_callable< func_type_v0(), void  >(),  "");
  static_assert( ! is_callable< func_type_v0(), void* >(),  "");
  static_assert( ! is_callable< func_type_v0(), int   >(),  "");

  static_assert( ! is_callable< func_type_v0(int) >(),	      "");
  static_assert( ! is_callable< func_type_v0(int), void  >(), "");
  static_assert( ! is_callable< func_type_v0(int), void* >(), "");
  static_assert( ! is_callable< func_type_v0(int), int   >(), "");

  using func_type_i0 = int(*)();

  static_assert(   is_callable< func_type_i0() >(),	  "");
  static_assert(   is_callable< func_type_i0(), void >(), "");
  static_assert(   is_callable< func_type_i0(), int  >(), "");
  static_assert( ! is_callable< func_type_i0(), int& >(), "");
  static_assert(   is_callable< func_type_i0(), long >(), "");

  static_assert( ! is_callable< func_type_i0(int) >(),	     "");
  static_assert( ! is_callable< func_type_i0(int), void >(), "");
  static_assert( ! is_callable< func_type_i0(int), int  >(), "");
  static_assert( ! is_callable< func_type_i0(int), int& >(), "");
  static_assert( ! is_callable< func_type_i0(int), long >(), "");

  using func_type_l0 = int&(*)();

  static_assert(   is_callable< func_type_l0() >(),	    "");
  static_assert(   is_callable< func_type_l0(), void >(),   "");
  static_assert(   is_callable< func_type_l0(), int >(),    "");
  static_assert(   is_callable< func_type_l0(), int& >(),   "");
  static_assert( ! is_callable< func_type_l0(), int&& >(),  "");
  static_assert(   is_callable< func_type_l0(), long >(),   "");
  static_assert( ! is_callable< func_type_l0(), long& >(),  "");

  static_assert( ! is_callable< func_type_l0(int) >(),	      "");
  static_assert( ! is_callable< func_type_l0(int), void >(),  "");
  static_assert( ! is_callable< func_type_l0(int), int  >(),  "");
  static_assert( ! is_callable< func_type_l0(int), int& >(),  "");
  static_assert( ! is_callable< func_type_l0(int), long >(),  "");

  using func_type_ii = int(*)(int);

  static_assert( ! is_callable< func_type_ii() >(),	  "");
  static_assert( ! is_callable< func_type_ii(), int  >(), "");
  static_assert( ! is_callable< func_type_ii(), int& >(), "");
  static_assert( ! is_callable< func_type_ii(), long >(), "");

  static_assert(   is_callable< func_type_ii(int) >(),	      "");
  static_assert(   is_callable< func_type_ii(int), int  >(),  "");
  static_assert( ! is_callable< func_type_ii(int), int& >(),  "");
  static_assert(   is_callable< func_type_ii(int), long >(),  "");

  using func_type_il = int(*)(int&);

  static_assert( ! is_callable< func_type_il() >(),	  "");

  static_assert( ! is_callable< func_type_il(int) >(),	      "");
  static_assert( ! is_callable< func_type_il(int), int  >(),  "");
  static_assert( ! is_callable< func_type_il(int), int& >(),  "");
  static_assert( ! is_callable< func_type_il(int), long >(),  "");

  static_assert(   is_callable< func_type_il(int&) >(),	      "");
  static_assert(   is_callable< func_type_il(int&), int  >(), "");
  static_assert( ! is_callable< func_type_il(int&), int& >(), "");
  static_assert(   is_callable< func_type_il(int&), long >(), "");

  using func_type_ir = int(*)(int&&);

  static_assert( ! is_callable< func_type_ir() >(),	  "");

  static_assert(   is_callable< func_type_ir(int) >(),	      "");
  static_assert(   is_callable< func_type_ir(int), int  >(),  "");
  static_assert( ! is_callable< func_type_ir(int), int& >(),  "");
  static_assert(   is_callable< func_type_ir(int), long >(),  "");

  static_assert( ! is_callable< func_type_ir(int&) >(),	      "");
  static_assert( ! is_callable< func_type_ir(int&), int  >(), "");
  static_assert( ! is_callable< func_type_ir(int&), int& >(), "");
  static_assert( ! is_callable< func_type_ir(int&), long >(), "");

  struct X { };

  using mem_type_i = int X::*;

  static_assert( ! is_callable< mem_type_i() >(),	  "");

  static_assert( ! is_callable< mem_type_i(int) >(),	    "");
  static_assert( ! is_callable< mem_type_i(int), int  >(),  "");
  static_assert( ! is_callable< mem_type_i(int), int& >(),  "");
  static_assert( ! is_callable< mem_type_i(int), long >(),  "");

  static_assert( ! is_callable< mem_type_i(int&) >(),	    "");
  static_assert( ! is_callable< mem_type_i(int&), int  >(), "");
  static_assert( ! is_callable< mem_type_i(int&), int& >(), "");
  static_assert( ! is_callable< mem_type_i(int&), long >(), "");

  static_assert(   is_callable< mem_type_i(X&) >(),	  "");
  static_assert(   is_callable< mem_type_i(X&), int  >(), "");
  static_assert(   is_callable< mem_type_i(X&), int& >(), "");
  static_assert(   is_callable< mem_type_i(X&), long >(), "");

  using memfun_type_i = int (X::*)();

  static_assert( ! is_callable< memfun_type_i() >(),	 "");

  static_assert( ! is_callable< memfun_type_i(int) >(),	 "");

  static_assert( ! is_callable< memfun_type_i(int&) >(), "");

  static_assert(   is_callable< memfun_type_i(X&) >(),	      "");
  static_assert(   is_callable< memfun_type_i(X&), int  >(),  "");
  static_assert( ! is_callable< memfun_type_i(X&), int& >(),  "");
  static_assert(   is_callable< memfun_type_i(X&), long >(),  "");
  static_assert(   is_callable< memfun_type_i(X*) >(),	      "");

  static_assert( ! is_callable< memfun_type_i(const X&) >(),	      "");
  static_assert( ! is_callable< memfun_type_i(const X&), int  >(),  "");
  static_assert( ! is_callable< memfun_type_i(X&, int) >(), "");

  using memfun_type_iic = int& (X::*)(int&) const;

  static_assert( ! is_callable< memfun_type_iic() >(),		      "");
  static_assert( ! is_callable< memfun_type_iic(int)  >(),	      "");
  static_assert( ! is_callable< memfun_type_iic(int&) >(),	      "");
  static_assert( ! is_callable< memfun_type_iic(X&, int) >(),	      "");
  static_assert( ! is_callable< memfun_type_iic(const X&, int) >(),  "");
  static_assert( ! is_callable< memfun_type_iic(const X&, int&, int)  >(), "");

  static_assert(   is_callable< memfun_type_iic(const X&, int&)  >(),	   "");
  static_assert(   is_callable< memfun_type_iic(const X&, int&), int  >(), "");
  static_assert(   is_callable< memfun_type_iic(const X&, int&), int& >(), "");
  static_assert(   is_callable< memfun_type_iic(const X&, int&), long >(), "");
  static_assert( ! is_callable< memfun_type_iic(const X&, int&), long& >(),"");
  static_assert(   is_callable< memfun_type_iic(const X*, int&)  >(),	   "");

  struct F {
    int& operator()();
    long& operator()() const;
    short& operator()(int) &&;
    char& operator()(int) const&;
  private:
    void operator()(int, int);
  };
  using CF = const F;

  static_assert(   is_callable< F(),   int&   >(), "");
  static_assert(   is_callable< F&(),  int&   >(), "");
  static_assert(   is_callable< CF(),  long& >(), "");
  static_assert(   is_callable< CF&(), long& >(), "");
  static_assert(   is_callable< F(int),	  short& >(), "");
  static_assert(   is_callable< F&(int),  char& >(), "");
  static_assert(   is_callable< CF(int),  char& >(), "");
  static_assert(   is_callable< CF&(int), char& >(), "");

  static_assert( ! is_callable< F(int, int) >(), "");
}
