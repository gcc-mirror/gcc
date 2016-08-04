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

#ifndef IS_NT_CALLABLE_DEFINED
template<typename T, typename R = void>
  constexpr bool is_nt_callable()
  {
    static_assert(std::is_nothrow_callable<T, R>::value
                  == std::is_nothrow_callable_v<T, R>);
    return std::is_nothrow_callable_v<T, R>;
  }
#endif

void test01()
{
  using func_type = void(*)();
  static_assert( ! is_nt_callable< func_type() >(),     "");

#if __cpp_noexcept_function_type
  using func_type_nt = void(*)() noexcept;
  static_assert(   is_nt_callable< func_type_nt() >(),  "");
#endif

  struct X { };
  using mem_type = int X::*;

  static_assert( ! is_nt_callable< mem_type() >(),	"");
  static_assert( ! is_nt_callable< mem_type(int) >(),   "");
  static_assert( ! is_nt_callable< mem_type(int&) >(),	"");

  static_assert(   is_nt_callable< mem_type(X&) >(),       "");
  static_assert(   is_nt_callable< mem_type(X&), int  >(), "");
  static_assert(   is_nt_callable< mem_type(X&), int& >(), "");
  static_assert(   is_nt_callable< mem_type(X&), long >(), "");
  static_assert(   is_nt_callable< mem_type(X*), int& >(), "");

  using memfun_type = int (X::*)();

  static_assert( ! is_nt_callable< memfun_type() >(),     "");
  static_assert( ! is_nt_callable< memfun_type(int) >(),  "");
  static_assert( ! is_nt_callable< memfun_type(int&) >(), "");
  static_assert( ! is_nt_callable< memfun_type(X&) >(),   "");
  static_assert( ! is_nt_callable< memfun_type(X*) >(),   "");

#if __cpp_noexcept_function_type
  using memfun_type_nt = int (X::*)() noexcept;

  static_assert( ! is_nt_callable< memfun_type_nt() >(),	    "");
  static_assert( ! is_nt_callable< memfun_type_nt(int) >(),  "");
  static_assert( ! is_nt_callable< memfun_type_nt(int&) >(), "");
  static_assert(   is_nt_callable< memfun_type_nt(X&) >(),   "");
  static_assert(   is_nt_callable< memfun_type_nt(X*) >(),   "");
#endif

  struct F {
    int& operator()();
    long& operator()() const noexcept;
    short& operator()(int) &&;
    char& operator()(int) const& noexcept;
  private:
    void operator()(int, int) noexcept;
  };
  using CF = const F;

  static_assert( ! is_nt_callable< F(),   int&   >(), "");
  static_assert(   is_nt_callable< CF(),  long& >(),  "");
  static_assert( ! is_nt_callable< F(int),    short& >(), "");
  static_assert(   is_nt_callable< F&(int),   char& >(),  "");
  static_assert(   is_nt_callable< CF(int),   char& >(),  "");
  static_assert(   is_nt_callable< CF&(int),  char& >(),  "");

  static_assert( ! is_nt_callable< F(int, int) >(), "");
}
