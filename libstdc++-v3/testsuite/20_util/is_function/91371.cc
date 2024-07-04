// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

using std::is_function;

#ifdef __i386__
static_assert(is_function<void __attribute__((thiscall)) ()>::value, "");
static_assert(is_function<void __attribute__((thiscall)) () const>::value, "");
static_assert(is_function<void __attribute__((fastcall)) ()>::value, "");
static_assert(is_function<void __attribute__((fastcall)) () const>::value, "");
static_assert(is_function<void __attribute__((stdcall)) ()>::value, "");
static_assert(is_function<void __attribute__((stdcall)) () const>::value, "");
#endif
static_assert(is_function<void __attribute__((ms_abi)) ()>::value, "");
static_assert(is_function<void __attribute__((ms_abi)) () const>::value, "");
static_assert(is_function<void __attribute__((ms_abi)) () const &>::value, "");
static_assert(is_function<void __attribute__((ms_abi)) () &&>::value, "");
static_assert(is_function<void __attribute__((sysv_abi)) ()>::value, "");
static_assert(is_function<void __attribute__((sysv_abi)) () const>::value, "");

struct X { operator X*(); };
static_assert(!is_function<X>::value, "");
static_assert(!is_function<X&>::value, "");
static_assert(!is_function<X*>::value, "");
union Y { operator Y*(); int i; long l;};
static_assert(!is_function<Y>::value, "");
static_assert(!is_function<Y&>::value, "");
static_assert(!is_function<Y*>::value, "");
