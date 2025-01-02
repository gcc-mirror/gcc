// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target { { i?86-*-* x86_64-*-* } && { ! x32 } } } }
// { dg-require-effective-target c++11 }

#include <type_traits>

struct Z
{
  void __attribute__((ms_abi)) f() const { }
  void __attribute__((sysv_abi)) g() const { }
#ifdef __i386__
  void __attribute__((thiscall)) h() const { }
#endif
};
static_assert( std::is_member_function_pointer<decltype(&Z::f)>::value, "" );
static_assert( std::is_member_function_pointer<decltype(&Z::g)>::value, "" );
#ifdef __i386__
static_assert( std::is_member_function_pointer<decltype(&Z::h)>::value, "" );
#endif
