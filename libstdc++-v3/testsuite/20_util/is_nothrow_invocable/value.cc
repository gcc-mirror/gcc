// Copyright (C) 2016-2017 Free Software Foundation, Inc.
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

#ifndef IS_NT_INVOCABLE_DEFINED
template<typename... T>
  constexpr bool is_nt_invocable()
  {
    constexpr bool result = std::is_nothrow_invocable_v<T...>;
    static_assert(std::is_nothrow_invocable<T...>::value == result);
    return result;
  }

template<typename R, typename... T>
  constexpr bool is_nt_invocable_r()
  {
    constexpr bool result = std::is_nothrow_invocable_r_v<R, T...>;
    static_assert(std::is_nothrow_invocable_r<R, T...>::value == result);
    return result;
  }
#endif

void test01()
{
  struct T { T(int) { } };
  struct NT { NT(int) noexcept { } };
  struct Ex { explicit Ex(int) noexcept { } };

  using func_type = void(*)();
  static_assert( ! is_nt_invocable< func_type>(),     "");

#if __cpp_noexcept_function_type
  using func_type_nt = void(*)() noexcept;
  static_assert(   is_nt_invocable< func_type_nt >(),  "");
#endif

  struct X { };
  using mem_type = int X::*;

  static_assert( ! is_nt_invocable< mem_type >(),	"");
  static_assert( ! is_nt_invocable< mem_type, int >(),   "");
  static_assert( ! is_nt_invocable< mem_type, int& >(),	"");

  static_assert(   is_nt_invocable< mem_type, X& >(),          "");
  static_assert(   is_nt_invocable_r< int,   mem_type, X& >(), "");
  static_assert(   is_nt_invocable_r< int&,  mem_type, X& >(), "");
  static_assert(   is_nt_invocable_r< long,  mem_type, X& >(), "");
  static_assert( ! is_nt_invocable_r< long&, mem_type, X& >(),
		   "conversion fails, cannot bind long& to int");
  static_assert(   is_nt_invocable_r< int&,  mem_type, X* >(), "");

  static_assert( ! is_nt_invocable_r< T,  mem_type, X& >(),
		   "conversion throws");
  static_assert(   is_nt_invocable_r< NT, mem_type, X& >(), "");
  static_assert( ! is_nt_invocable_r< Ex, mem_type, X& >(),
		   "conversion fails, would use explicit constructor");

  using memfun_type = int (X::*)();

  static_assert( ! is_nt_invocable< memfun_type >(),       "no object");
  static_assert( ! is_nt_invocable< memfun_type, int >(),  "no object");
  static_assert( ! is_nt_invocable< memfun_type, int& >(), "no object");
  static_assert( ! is_nt_invocable< memfun_type, X& >(),   "call throws");
  static_assert( ! is_nt_invocable< memfun_type, X* >(),   "call throws");

  static_assert( ! is_nt_invocable_r< T,  memfun_type, X& >(), "call throws");
  static_assert( ! is_nt_invocable_r< NT, memfun_type, X& >(), "call throws");
  static_assert( ! is_nt_invocable_r< Ex, memfun_type, X& >(), "call throws");

#if __cpp_noexcept_function_type
  using memfun_type_nt = int (X::*)() noexcept;

  static_assert( ! is_nt_invocable< memfun_type_nt >(),	      "no object");
  static_assert( ! is_nt_invocable< memfun_type_nt, int >(),  "no object");
  static_assert( ! is_nt_invocable< memfun_type_nt, int& >(), "no object");
  static_assert(   is_nt_invocable< memfun_type_nt, X& >(),   "");
  static_assert(   is_nt_invocable< memfun_type_nt, X* >(),   "");

  static_assert( ! is_nt_invocable_r< T,  memfun_type_nt, X& >(),
		   "conversion throws");
  static_assert(   is_nt_invocable_r< NT, memfun_type_nt, X& >(), "");
  static_assert( ! is_nt_invocable_r< Ex, memfun_type_nt, X& >(),
		   "conversion fails, would use explicit constructor");
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

  static_assert( ! is_nt_invocable< F  >(), "call throws");
  static_assert(   is_nt_invocable< CF >(), "");

  static_assert( ! is_nt_invocable_r< int&,  F  >(), "call throws");
  static_assert(   is_nt_invocable_r< long&, CF >(), "");
  static_assert( ! is_nt_invocable_r< T,     F  >(), "call throws");
  static_assert( ! is_nt_invocable_r< NT,    F  >(), "call throws");
  static_assert( ! is_nt_invocable_r< Ex,    F  >(), "call throws");
  static_assert( ! is_nt_invocable_r< T,     CF >(), "conversion throws");
  static_assert(   is_nt_invocable_r< NT,    CF >(), "" );
  static_assert( ! is_nt_invocable_r< Ex,    CF >(), "conversion fails");

  static_assert( ! is_nt_invocable< F,   int >(), "call throws");
  static_assert(   is_nt_invocable< F&,  int >(), "");

  static_assert( ! is_nt_invocable_r< short&, F,   int >(),
		   "call throws" );
  static_assert(   is_nt_invocable_r< char&,  F&,  int >(), "");
  static_assert( ! is_nt_invocable_r< T,      F&,  int >(),
		   "conversion throws");
  static_assert(   is_nt_invocable_r< NT,     F&,  int >(), "");
  static_assert( ! is_nt_invocable_r< Ex,     F&,  int >(),
		   "conversion fails, would use explicit constructor");

  static_assert(   is_nt_invocable< CF,   int >(), "");
  static_assert(   is_nt_invocable< CF&,  int >(), "");

  static_assert(   is_nt_invocable_r< char&,  CF,  int >(), "");
  static_assert(   is_nt_invocable_r< char&,  CF&, int >(), "");

  static_assert( ! is_nt_invocable_r< T,      CF&, int >(),
		   "conversion throws");
  static_assert(   is_nt_invocable_r< NT,     CF&, int >(), "");
  static_assert( ! is_nt_invocable_r< Ex,     CF&, int >(),
		   "conversion fails, would use explicit constructor");

  static_assert( ! is_nt_invocable< F, int, int >(),
		   "would call private member");
  static_assert( ! is_nt_invocable_r<void, F, int, int >(),
		   "would call private member");
}
