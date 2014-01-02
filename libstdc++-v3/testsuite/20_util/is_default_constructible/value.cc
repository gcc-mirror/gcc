// { dg-options "-std=gnu++0x" }
// { dg-do compile }

// Copyright (C) 2011-2014 Free Software Foundation, Inc.
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

using namespace __gnu_test::construct;

static_assert(std::is_default_constructible<int>::value, "Error");
static_assert(std::is_default_constructible<int const>::value, "Error");
static_assert(std::is_default_constructible<int const volatile>::value,
	      "Error");
static_assert(std::is_default_constructible<int*>::value, "Error");
static_assert(std::is_default_constructible<void*>::value, "Error");
static_assert(std::is_default_constructible<void* const>::value, "Error");
static_assert(std::is_default_constructible<int B::*>::value, "Error");
static_assert(std::is_default_constructible<void(*)()>::value, "Error");
static_assert(std::is_default_constructible<std::nullptr_t>::value, "Error");
static_assert(std::is_default_constructible<std::nullptr_t const>::value,
	      "Error");
static_assert(std::is_default_constructible<Empty>::value, "Error");
static_assert(std::is_default_constructible<Empty const>::value, "Error");
static_assert(std::is_default_constructible<FromArgs<>>::value, "Error");
static_assert(std::is_default_constructible<FromArgs<> const>::value, "Error");
static_assert(std::is_default_constructible<nAny>::value, "Error");
static_assert(std::is_default_constructible<nAny const>::value, "Error");
static_assert(std::is_default_constructible<Ellipsis>::value, "Error");
static_assert(std::is_default_constructible<Ellipsis const>::value, "Error");
static_assert(std::is_default_constructible<U>::value, "Error");
static_assert(std::is_default_constructible<U const>::value, "Error");
static_assert(std::is_default_constructible<E>::value, "Error");
static_assert(std::is_default_constructible<E const>::value, "Error");
static_assert(std::is_default_constructible<SE>::value, "Error");
static_assert(std::is_default_constructible<SE const>::value, "Error");
static_assert(std::is_default_constructible<OpE>::value, "Error");
static_assert(std::is_default_constructible<OpE const>::value, "Error");
static_assert(std::is_default_constructible<OpSE>::value, "Error");
static_assert(std::is_default_constructible<OpSE const>::value, "Error");
static_assert(std::is_default_constructible<int[1]>::value, "Error");
static_assert(std::is_default_constructible<const int[1]>::value, "Error");
static_assert(std::is_default_constructible<int[1][2]>::value, "Error");
static_assert(std::is_default_constructible<const int[1][2]>::value, "Error");
static_assert(std::is_default_constructible<FromArgs<>[1]>::value, "Error");
static_assert(std::is_default_constructible<const FromArgs<>[1]>::value,
	      "Error");
static_assert(std::is_default_constructible<U[1]>::value, "Error");
static_assert(std::is_default_constructible<const U[1]>::value, "Error");
static_assert(std::is_default_constructible<Empty[1]>::value, "Error");
static_assert(std::is_default_constructible<const Empty[1]>::value, "Error");
static_assert(std::is_default_constructible<Ellipsis[1]>::value, "Error");
static_assert(std::is_default_constructible<const Ellipsis[1]>::value, "Error");
static_assert(std::is_default_constructible<std::nullptr_t[1]>::value, "Error");
static_assert(std::is_default_constructible<const std::nullptr_t[1]>::value,
	      "Error");
static_assert(std::is_default_constructible<nAny[1]>::value, "Error");
static_assert(std::is_default_constructible<const nAny[1]>::value, "Error");
static_assert(std::is_default_constructible<E[1]>::value, "Error");
static_assert(std::is_default_constructible<const E[1]>::value, "Error");
static_assert(std::is_default_constructible<SE[1]>::value, "Error");
static_assert(std::is_default_constructible<const SE[1]>::value, "Error");
static_assert(std::is_default_constructible<OpE[1]>::value, "Error");
static_assert(std::is_default_constructible<const OpE[1]>::value, "Error");
static_assert(std::is_default_constructible<OpSE[1]>::value, "Error");
static_assert(std::is_default_constructible<const OpSE[1]>::value, "Error");
static_assert(std::is_default_constructible<int*[1]>::value, "Error");
static_assert(std::is_default_constructible<int* const[1]>::value, "Error");
static_assert(std::is_default_constructible<int B::*[1]>::value, "Error");
static_assert(std::is_default_constructible<int B::* const[1]>::value, "Error");
static_assert(std::is_default_constructible<std::initializer_list<int>>::value,
	      "Error");
static_assert(std::is_default_constructible<const
	      std::initializer_list<int>>::value, "Error");
static_assert(std::is_default_constructible<
	      std::initializer_list<int>[1]>::value, "Error");
static_assert(std::is_default_constructible<const
	      std::initializer_list<int>[1]>::value, "Error");

static_assert(std::is_default_constructible
	      <__gnu_test::NoexceptDefaultClass>::value, "Error");
static_assert(std::is_default_constructible
	      <__gnu_test::ThrowDefaultClass>::value, "Error");
static_assert(std::is_default_constructible
	      <__gnu_test::ExceptDefaultClass>::value, "Error");

static_assert(!std::is_default_constructible<void>::value, "Error");
static_assert(!std::is_default_constructible<const void>::value, "Error");
static_assert(!std::is_default_constructible<Abstract>::value, "Error");
static_assert(!std::is_default_constructible<const Abstract>::value, "Error");
static_assert(!std::is_default_constructible<Any>::value, "Error");
static_assert(!std::is_default_constructible<const Any>::value, "Error");
static_assert(!std::is_default_constructible<FromArgs<int>>::value, "Error");
static_assert(!std::is_default_constructible<const FromArgs<int>>::value,
	      "Error");
static_assert(!std::is_default_constructible<int&>::value, "Error");
static_assert(!std::is_default_constructible<int&&>::value, "Error");
static_assert(!std::is_default_constructible<void()>::value, "Error");
static_assert(!std::is_default_constructible<void() const volatile>::value,
	      "Error");
static_assert(!std::is_default_constructible<void(&)()>::value, "Error");
static_assert(!std::is_default_constructible<int(&)[1]>::value, "Error");
static_assert(!std::is_default_constructible<int(&)[]>::value, "Error");
static_assert(!std::is_default_constructible<int[]>::value, "Error");
static_assert(!std::is_default_constructible<const int[]>::value, "Error");
static_assert(!std::is_default_constructible<int[][1][2]>::value, "Error");
static_assert(!std::is_default_constructible<const int[][1][2]>::value,
	      "Error");
static_assert(!std::is_default_constructible<Any[1]>::value, "Error");
static_assert(!std::is_default_constructible<const Any[1]>::value, "Error");
static_assert(!std::is_default_constructible<FromArgs<int>[1]>::value, "Error");
static_assert(!std::is_default_constructible<const FromArgs<int>[1]>::value,
	      "Error");
static_assert(!std::is_default_constructible<
	      FromArgs<std::initializer_list<int>>>::value, "Error");
static_assert(!std::is_default_constructible<const
	      FromArgs<std::initializer_list<int>>>::value, "Error");
static_assert(!std::is_default_constructible<const
	      FromArgs<const std::initializer_list<int>>>::value, "Error");
static_assert(!std::is_default_constructible<DelDef>::value, "Error");
static_assert(!std::is_default_constructible<const DelDef>::value, "Error");
static_assert(!std::is_default_constructible<DelCopy>::value, "Error");
static_assert(!std::is_default_constructible<const DelCopy>::value, "Error");
static_assert(!std::is_default_constructible<DelDtor>::value, "Error");
static_assert(!std::is_default_constructible<const DelDtor>::value, "Error");
