// { dg-options "-std=gnu++0x" }
// { dg-do compile }

// Copyright (C) 2011 Free Software Foundation, Inc.
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

using namespace __gnu_test::construct_destruct;

static_assert(std::is_destructible<int>::value, "Error");
static_assert(std::is_destructible<const int>::value, "Error");
static_assert(std::is_destructible<bool>::value, "Error");
static_assert(std::is_destructible<const bool>::value, "Error");
static_assert(std::is_destructible<int*>::value, "Error");
static_assert(std::is_destructible<void*>::value, "Error");
static_assert(std::is_destructible<int B::*>::value, "Error");
static_assert(std::is_destructible<const int D::*>::value, "Error");
static_assert(std::is_destructible<E>::value, "Error");
static_assert(std::is_destructible<const E>::value, "Error");
static_assert(std::is_destructible<SE>::value, "Error");
static_assert(std::is_destructible<const SE>::value, "Error");
static_assert(std::is_destructible<OpE>::value, "Error");
static_assert(std::is_destructible<const OpE>::value, "Error");
static_assert(std::is_destructible<OpSE>::value, "Error");
static_assert(std::is_destructible<const OpSE>::value, "Error");
static_assert(std::is_destructible<std::nullptr_t>::value, "Error");
static_assert(std::is_destructible<const std::nullptr_t>::value, "Error");
static_assert(std::is_destructible<B>::value, "Error");
static_assert(std::is_destructible<const B>::value, "Error");
static_assert(std::is_destructible<D>::value, "Error");
static_assert(std::is_destructible<const D>::value, "Error");
static_assert(std::is_destructible<Empty>::value, "Error");
static_assert(std::is_destructible<const Empty>::value, "Error");
static_assert(std::is_destructible<U>::value, "Error");
static_assert(std::is_destructible<const U>::value, "Error");
static_assert(std::is_destructible<Abstract>::value, "Error");
static_assert(std::is_destructible<const Abstract>::value, "Error");
static_assert(std::is_destructible<int[1]>::value, "Error");
static_assert(std::is_destructible<const int[1]>::value, "Error");
static_assert(std::is_destructible<int[1][2]>::value, "Error");
static_assert(std::is_destructible<const int[1][2]>::value, "Error");
static_assert(std::is_destructible<int&>::value, "Error");
static_assert(std::is_destructible<int&&>::value, "Error");
static_assert(std::is_destructible<void(&)()>::value, "Error");
static_assert(std::is_destructible<Ellipsis>::value, "Error");
static_assert(std::is_destructible<const Ellipsis>::value, "Error");
static_assert(std::is_destructible<Any>::value, "Error");
static_assert(std::is_destructible<const Any>::value, "Error");
static_assert(std::is_destructible<nAny>::value, "Error");
static_assert(std::is_destructible<const nAny>::value, "Error");
static_assert(std::is_destructible<DelDef>::value, "Error");
static_assert(std::is_destructible<const DelDef>::value, "Error");
static_assert(std::is_destructible<DelCopy>::value, "Error");
static_assert(std::is_destructible<const DelCopy>::value, "Error");
static_assert(std::is_destructible<DelEllipsis>::value, "Error");
static_assert(std::is_destructible<const DelEllipsis>::value, "Error");
static_assert(std::is_destructible<std::initializer_list<int>>::value,
	      "Error");
static_assert(std::is_destructible<const std::initializer_list<int>>::value,
	      "Error");
static_assert(std::is_destructible<void()>::value, "Error");
static_assert(std::is_destructible<void() const>::value, "Error");

static_assert(!std::is_destructible<void>::value, "Error");
static_assert(!std::is_destructible<const void>::value, "Error");
static_assert(!std::is_destructible<int[]>::value, "Error");
static_assert(!std::is_destructible<const int[]>::value, "Error");
static_assert(!std::is_destructible<DelDtor>::value, "Error");
static_assert(!std::is_destructible<const DelDtor>::value, "Error");
static_assert(!std::is_destructible<AbstractDelDtor>::value, "Error");
static_assert(!std::is_destructible<const AbstractDelDtor>::value, "Error");
static_assert(!std::is_destructible<int[][1]>::value, "Error");
static_assert(!std::is_destructible<const int[][1]>::value, "Error");
static_assert(!std::is_destructible<DelDtor[1]>::value, "Error");
static_assert(!std::is_destructible<const DelDtor[1]>::value, "Error");
static_assert(!std::is_destructible<DelDtor[]>::value, "Error");
static_assert(!std::is_destructible<const DelDtor[]>::value, "Error");

// Deleted members in unions with non-trivial members:
static_assert(!std::is_destructible<NontrivialUnion>::value, "Error");

// Unusual copy:
static_assert(std::is_destructible<UnusualCopy>::value, "Error");
