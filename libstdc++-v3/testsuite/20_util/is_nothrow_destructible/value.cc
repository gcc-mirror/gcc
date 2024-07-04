// { dg-do compile { target c++11 } }

// Copyright (C) 2012-2024 Free Software Foundation, Inc.
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

#include <type_traits>
#include <initializer_list>
#include <testsuite_tr1.h>

using namespace __gnu_test::destruct;

// is_nothrow_destructible:
static_assert(std::is_nothrow_destructible<int>::value, "Error");
static_assert(std::is_nothrow_destructible<const int>::value, "Error");
static_assert(std::is_nothrow_destructible<const volatile int>::value, "Error");
static_assert(std::is_nothrow_destructible<int[12]>::value, "Error");
static_assert(std::is_nothrow_destructible<const int[12]>::value, "Error");
static_assert(std::is_nothrow_destructible<const volatile int[12]>::value, "Error");
static_assert(std::is_nothrow_destructible<decltype(nullptr)>::value, "Error");
static_assert(std::is_nothrow_destructible<std::initializer_list<int>>::value, "Error");
static_assert(std::is_nothrow_destructible<std::initializer_list<decltype(nullptr)>>::value, "Error");
static_assert(std::is_nothrow_destructible<std::initializer_list<TD1>>::value, "Error");
static_assert(std::is_nothrow_destructible<std::initializer_list<TD2>>::value, "Error");
static_assert(std::is_nothrow_destructible<E>::value, "Error");
static_assert(std::is_nothrow_destructible<const E>::value, "Error");
static_assert(std::is_nothrow_destructible<const volatile E>::value, "Error");
static_assert(std::is_nothrow_destructible<NTD1>::value, "Error");
static_assert(std::is_nothrow_destructible<NTD2>::value, "Error");
static_assert(std::is_nothrow_destructible<NTD3>::value, "Error");
static_assert(std::is_nothrow_destructible<Aggr>::value, "Error");
static_assert(std::is_nothrow_destructible<U1>::value, "Error");
static_assert(std::is_nothrow_destructible<void(*)()>::value, "Error");
static_assert(std::is_nothrow_destructible<void*>::value, "Error");
static_assert(std::is_nothrow_destructible<int&>::value, "Error");
static_assert(std::is_nothrow_destructible<TD1&>::value, "Error");
static_assert(std::is_nothrow_destructible<TD2&>::value, "Error");
static_assert(std::is_nothrow_destructible<TD1*>::value, "Error");
static_assert(std::is_nothrow_destructible<TD2*>::value, "Error");
static_assert(std::is_nothrow_destructible<void(&)()>::value, "Error");
static_assert(std::is_nothrow_destructible<void(&&)()>::value, "Error");
static_assert(std::is_nothrow_destructible<En>::value, "Error");
static_assert(std::is_nothrow_destructible<En*>::value, "Error");
static_assert(std::is_nothrow_destructible<En&>::value, "Error");
static_assert(std::is_nothrow_destructible<En2>::value, "Error");
static_assert(std::is_nothrow_destructible<En2*>::value, "Error");
static_assert(std::is_nothrow_destructible<En2&>::value, "Error");
static_assert(std::is_nothrow_destructible<TD1(&)(Aggr2, TD2)>::value, "Error");
static_assert(std::is_nothrow_destructible<TD1(*)(Aggr2, TD2)>::value, "Error");
static_assert(std::is_nothrow_destructible<Abstract1>::value, "Error");
static_assert(std::is_nothrow_destructible<Der>::value, "Error");
static_assert(std::is_nothrow_destructible<Del&>::value, "Error");
static_assert(std::is_nothrow_destructible<Del2&>::value, "Error");
static_assert(std::is_nothrow_destructible<Del3&>::value, "Error");
static_assert(std::is_nothrow_destructible<Del(&)[1]>::value, "Error");
static_assert(std::is_nothrow_destructible<Del2(&)[2]>::value, "Error");
static_assert(std::is_nothrow_destructible<Del3(&)[3]>::value, "Error");
static_assert(std::is_nothrow_destructible<Del&&>::value, "Error");
static_assert(std::is_nothrow_destructible<Del2&&>::value, "Error");
static_assert(std::is_nothrow_destructible<Del3&>::value, "Error");
static_assert(std::is_nothrow_destructible<Del(&&)[1]>::value, "Error");
static_assert(std::is_nothrow_destructible<Del2(&&)[2]>::value, "Error");
static_assert(std::is_nothrow_destructible<Del3(&&)[3]>::value, "Error");
static_assert(std::is_nothrow_destructible<Ut&>::value, "Error");
static_assert(std::is_nothrow_destructible<Ut&&>::value, "Error");
static_assert(std::is_nothrow_destructible<Ut*>::value, "Error");
static_assert(std::is_nothrow_destructible<Abstract2&>::value, "Error");
static_assert(std::is_nothrow_destructible<Abstract3&>::value, "Error");
static_assert(std::is_nothrow_destructible<Abstract2*>::value, "Error");
static_assert(std::is_nothrow_destructible<Abstract3*>::value, "Error");

static_assert(!std::is_nothrow_destructible<void>::value, "Error");
static_assert(!std::is_nothrow_destructible<const void>::value, "Error");
static_assert(!std::is_nothrow_destructible<void()>::value, "Error");
static_assert(!std::is_nothrow_destructible<void() const>::value, "Error");
static_assert(!std::is_nothrow_destructible<TD1(Aggr2, TD2)>::value, "Error");
static_assert(!std::is_nothrow_destructible<int[]>::value, "Error");
static_assert(!std::is_nothrow_destructible<const int[]>::value, "Error");
static_assert(!std::is_nothrow_destructible<const volatile int[]>::value, "Error");
static_assert(!std::is_nothrow_destructible<int[][123]>::value, "Error");
static_assert(!std::is_nothrow_destructible<TD1>::value, "Error");
static_assert(!std::is_nothrow_destructible<TD2>::value, "Error");
static_assert(!std::is_nothrow_destructible<Aggr2>::value, "Error");
static_assert(!std::is_nothrow_destructible<Aggr2[1]>::value, "Error");
static_assert(!std::is_nothrow_destructible<TD1[1][2]>::value, "Error");
static_assert(!std::is_nothrow_destructible<Ut>::value, "Error");
static_assert(!std::is_nothrow_destructible<Ut[3]>::value, "Error");
static_assert(!std::is_nothrow_destructible<AbstractDelDtor>::value, "Error");
static_assert(!std::is_nothrow_destructible<Abstract2>::value, "Error");
static_assert(!std::is_nothrow_destructible<Abstract3>::value, "Error");
static_assert(!std::is_nothrow_destructible<Der2>::value, "Error");
static_assert(!std::is_nothrow_destructible<Del>::value, "Error");
static_assert(!std::is_nothrow_destructible<Del2>::value, "Error");
static_assert(!std::is_nothrow_destructible<Del3>::value, "Error");
static_assert(!std::is_nothrow_destructible<Del[1]>::value, "Error");
static_assert(!std::is_nothrow_destructible<Del2[2]>::value, "Error");
static_assert(!std::is_nothrow_destructible<Del3[3]>::value, "Error");

