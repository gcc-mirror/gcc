// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17 } }

#include <type_traits>

#ifndef IS_INVOCABLE_DEFINED
template<typename... T>
  constexpr bool is_invocable()
  {
    constexpr bool result = std::is_invocable_v<T...>;
    static_assert(std::is_invocable<T...>::value == result);
    return result;
  }

template<typename R, typename... T>
  constexpr bool is_invocable_r()
  {
    constexpr bool result = std::is_invocable_r_v<R, T...>;
    static_assert(std::is_invocable_r<R, T...>::value == result);
    return result;
  }
#endif

void test01()
{
  using func_type_v0 = void(*)();

  static_assert(   is_invocable< func_type_v0 >(),	    "");
  static_assert(   is_invocable_r<void,  func_type_v0 >(),  "");
  static_assert( ! is_invocable_r<void*, func_type_v0 >(),  "");
  static_assert( ! is_invocable_r<int,   func_type_v0 >(),  "");

  static_assert( ! is_invocable< func_type_v0, int >(),	      "");
  static_assert( ! is_invocable_r< void,  func_type_v0, int >(), "");
  static_assert( ! is_invocable_r< void*, func_type_v0, int >(), "");
  static_assert( ! is_invocable_r< int,   func_type_v0, int >(), "");

  using func_type_i0 = int(*)();

  static_assert(   is_invocable< func_type_i0 >(),	  "");
  static_assert(   is_invocable_r<void, func_type_i0 >(), "");
  static_assert(   is_invocable_r<int,  func_type_i0 >(), "");
  static_assert( ! is_invocable_r<int&, func_type_i0 >(), "");
  static_assert(   is_invocable_r<long, func_type_i0 >(), "");

  static_assert( ! is_invocable< func_type_i0, int >(),	     "");
  static_assert( ! is_invocable_r< void, func_type_i0, int >(), "");
  static_assert( ! is_invocable_r< int,  func_type_i0, int >(), "");
  static_assert( ! is_invocable_r< int&, func_type_i0, int >(), "");
  static_assert( ! is_invocable_r< long, func_type_i0, int >(), "");

  using func_type_l0 = int&(*)();

  static_assert(   is_invocable< func_type_l0 >(),	    "");
  static_assert(   is_invocable_r< void,  func_type_l0 >(),   "");
  static_assert(   is_invocable_r< int,   func_type_l0 >(),    "");
  static_assert(   is_invocable_r< int&,  func_type_l0 >(),   "");
  static_assert( ! is_invocable_r< int&&, func_type_l0 >(),  "");
  static_assert(   is_invocable_r< long,  func_type_l0 >(),   "");
  static_assert( ! is_invocable_r< long&, func_type_l0 >(),  "");

  static_assert( ! is_invocable< func_type_l0(int) >(),	      "");
  static_assert( ! is_invocable_r< void, func_type_l0, int >(),  "");
  static_assert( ! is_invocable_r< int,  func_type_l0, int >(),  "");
  static_assert( ! is_invocable_r< int&, func_type_l0, int >(),  "");
  static_assert( ! is_invocable_r< long, func_type_l0, int >(),  "");

  using func_type_ii = int(*)(int);

  static_assert( ! is_invocable< func_type_ii >(),	  "");
  static_assert( ! is_invocable_r< int,  func_type_ii >(), "");
  static_assert( ! is_invocable_r< int&, func_type_ii >(), "");
  static_assert( ! is_invocable_r< long, func_type_ii >(), "");

  static_assert(   is_invocable< func_type_ii, int >(),	      "");
  static_assert(   is_invocable_r< int,  func_type_ii, int >(),  "");
  static_assert( ! is_invocable_r< int&, func_type_ii, int >(),  "");
  static_assert(   is_invocable_r< long, func_type_ii, int >(),  "");

  using func_type_il = int(*)(int&);

  static_assert( ! is_invocable< func_type_il >(),	  "");

  static_assert( ! is_invocable< func_type_il, int >(),	      "");
  static_assert( ! is_invocable_r< int,  func_type_il, int >(),  "");
  static_assert( ! is_invocable_r< int&, func_type_il, int >(),  "");
  static_assert( ! is_invocable_r< long, func_type_il, int >(),  "");

  static_assert(   is_invocable< func_type_il, int& >(),	      "");
  static_assert(   is_invocable_r< int,  func_type_il, int& >(), "");
  static_assert( ! is_invocable_r< int&, func_type_il, int& >(), "");
  static_assert(   is_invocable_r< long, func_type_il, int& >(), "");

  using func_type_ir = int(*)(int&&);

  static_assert( ! is_invocable< func_type_ir >(),	  "");

  static_assert(   is_invocable< func_type_ir, int >(),	      "");
  static_assert(   is_invocable_r< int,  func_type_ir, int >(),  "");
  static_assert( ! is_invocable_r< int&, func_type_ir, int >(),  "");
  static_assert(   is_invocable_r< long, func_type_ir, int >(),  "");

  static_assert( ! is_invocable< func_type_ir, int& >(),	      "");
  static_assert( ! is_invocable_r< int,  func_type_ir, int& >(), "");
  static_assert( ! is_invocable_r< int&, func_type_ir, int& >(), "");
  static_assert( ! is_invocable_r< long, func_type_ir, int& >(), "");

  struct X { };

  using mem_type_i = int X::*;

  static_assert( ! is_invocable< mem_type_i >(),	  "");

  static_assert( ! is_invocable< mem_type_i, int >(),	    "");
  static_assert( ! is_invocable_r< int,  mem_type_i, int >(),  "");
  static_assert( ! is_invocable_r< int&, mem_type_i, int >(),  "");
  static_assert( ! is_invocable_r< long, mem_type_i, int >(),  "");

  static_assert( ! is_invocable< mem_type_i, int& >(),	    "");
  static_assert( ! is_invocable_r< int,  mem_type_i, int& >(), "");
  static_assert( ! is_invocable_r< int&, mem_type_i, int& >(), "");
  static_assert( ! is_invocable_r< long, mem_type_i, int& >(), "");

  static_assert(   is_invocable< mem_type_i, X& >(),	  "");
  static_assert(   is_invocable_r< int,  mem_type_i, X& >(), "");
  static_assert(   is_invocable_r< int&, mem_type_i, X& >(), "");
  static_assert(   is_invocable_r< long, mem_type_i, X& >(), "");

  using memfun_type_i = int (X::*)();

  static_assert( ! is_invocable< memfun_type_i >(),	 "");

  static_assert( ! is_invocable< memfun_type_i, int >(),	 "");

  static_assert( ! is_invocable< memfun_type_i, int& >(), "");

  static_assert(   is_invocable< memfun_type_i, X& >(),	      "");
  static_assert(   is_invocable_r< int,  memfun_type_i, X& >(),  "");
  static_assert( ! is_invocable_r< int&, memfun_type_i, X& >(),  "");
  static_assert(   is_invocable_r< long, memfun_type_i, X& >(),  "");
  static_assert(   is_invocable< memfun_type_i, X* >(),	      "");

  static_assert( ! is_invocable< memfun_type_i, const X& >(),	      "");
  static_assert( ! is_invocable_r< int,  memfun_type_i, const X& >(),  "");
  static_assert( ! is_invocable< memfun_type_i, X&, int >(), "");

  using memfun_type_iic = int& (X::*)(int&) const;

  static_assert( ! is_invocable< memfun_type_iic >(),		      "");
  static_assert( ! is_invocable< memfun_type_iic, int  >(),	      "");
  static_assert( ! is_invocable< memfun_type_iic, int& >(),	      "");
  static_assert( ! is_invocable< memfun_type_iic, X&, int >(),	      "");
  static_assert( ! is_invocable< memfun_type_iic, const X&, int >(),  "");
  static_assert( ! is_invocable< memfun_type_iic, const X&, int&, int  >(), "");

  static_assert(   is_invocable< memfun_type_iic, const X&, int&  >(),	   "");
  static_assert(   is_invocable_r< int,  memfun_type_iic, const X&, int& >(), "");
  static_assert(   is_invocable_r< int&, memfun_type_iic, const X&, int& >(), "");
  static_assert(   is_invocable_r< long, memfun_type_iic, const X&, int& >(), "");
  static_assert( ! is_invocable_r< long&, memfun_type_iic, const X&, int& >(),"");
  static_assert(   is_invocable< memfun_type_iic, const X*, int&  >(),	   "");

  struct F {
    int& operator()();
    long& operator()() const;
    short& operator()(int) &&;
    char& operator()(int) const&;
  private:
    void operator()(int, int);
  };
  using CF = const F;

  static_assert(   is_invocable_r< int&,   F        >(), "");
  static_assert(   is_invocable_r< int&,   F&       >(), "");
  static_assert(   is_invocable_r< long&,  CF       >(), "");
  static_assert(   is_invocable_r< long&,  CF&      >(), "");
  static_assert(   is_invocable_r< short&, F,   int >(), "");
  static_assert(   is_invocable_r< char&,  F&,  int >(), "");
  static_assert(   is_invocable_r< char&,  CF,  int >(), "");
  static_assert(   is_invocable_r< char&,  CF&, int >(), "");

  static_assert( ! is_invocable< F, int, int >(), "");
}
