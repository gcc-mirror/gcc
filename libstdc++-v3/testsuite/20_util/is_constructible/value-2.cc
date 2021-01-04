// { dg-do compile { target c++11 } }

// Copyright (C) 2011-2021 Free Software Foundation, Inc.
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

static_assert(std::is_constructible<int, int>::value, "Error");
static_assert(std::is_constructible<std::nullptr_t, std::nullptr_t>::value,
	      "Error");
static_assert(std::is_constructible<E, E>::value, "Error");
static_assert(std::is_constructible<SE, SE>::value, "Error");
static_assert(std::is_constructible<OpE, OpE>::value, "Error");
static_assert(std::is_constructible<OpSE, OpSE>::value, "Error");
static_assert(std::is_constructible<Empty, Empty>::value, "Error");
static_assert(std::is_constructible<B, B>::value, "Error");
static_assert(std::is_constructible<U, U>::value, "Error");
static_assert(std::is_constructible<int B::*, int B::*>::value, "Error");
static_assert(std::is_constructible<Ellipsis, Ellipsis>::value, "Error");
static_assert(std::is_constructible<int*, int*>::value, "Error");
static_assert(std::is_constructible<void*, void*>::value, "Error");
static_assert(std::is_constructible<Any, Any>::value, "Error");
static_assert(std::is_constructible<nAny, nAny>::value, "Error");
static_assert(std::is_constructible<std::initializer_list<int>,
	      std::initializer_list<int>>::value, "Error");
static_assert(std::is_constructible<DelDef, DelDef>::value, "Error");

static_assert(!std::is_constructible<void, void>::value, "Error");
static_assert(!std::is_constructible<Abstract, Abstract>::value, "Error");
static_assert(!std::is_constructible<int[], int[]>::value, "Error");
static_assert(!std::is_constructible<int[1], int[1]>::value, "Error");
static_assert(!std::is_constructible<DelCopy, const DelCopy&>::value, "Error");
static_assert(!std::is_constructible<DelCopy, DelCopy&&>::value, "Error");
static_assert(!std::is_constructible<DelCopy, DelCopy>::value, "Error");
static_assert(!std::is_constructible<DelDtor, void>::value, "Error");
static_assert(!std::is_constructible<DelDtor, int>::value, "Error");
static_assert(!std::is_constructible<DelDtor, DelDtor>::value, "Error");
static_assert(!std::is_constructible<DelDtor, DelDtor&&>::value, "Error");
static_assert(!std::is_constructible<DelDtor, const DelDtor&>::value, "Error");

static_assert(std::is_constructible<DelEllipsis, const DelEllipsis&>::value,
	      "Error");
static_assert(std::is_constructible<DelEllipsis, DelEllipsis&&>::value,
	      "Error");
static_assert(std::is_constructible<DelEllipsis, DelEllipsis>::value, "Error");
static_assert(!std::is_constructible<DelEllipsis, void>::value, "Error");
static_assert(!std::is_constructible<DelEllipsis, std::nullptr_t>::value,
	      "Error");
static_assert(!std::is_constructible<DelEllipsis, B>::value, "Error");
static_assert(!std::is_constructible<DelEllipsis, Empty>::value, "Error");
static_assert(!std::is_constructible<DelEllipsis, E>::value, "Error");
static_assert(!std::is_constructible<DelEllipsis, SE>::value, "Error");
static_assert(!std::is_constructible<DelEllipsis, OpE>::value, "Error");
static_assert(!std::is_constructible<DelEllipsis, OpSE>::value, "Error");
static_assert(!std::is_constructible<DelEllipsis, void()>::value, "Error");
static_assert(!std::is_constructible<DelEllipsis, void() const>::value,
 	      "Error");
static_assert(!std::is_constructible<DelEllipsis, int[1]>::value, "Error");
static_assert(!std::is_constructible<DelEllipsis, int[]>::value, "Error");
static_assert(!std::is_constructible<DelEllipsis, int*>::value, "Error");
static_assert(!std::is_constructible<DelEllipsis, void*>::value, "Error");
static_assert(!std::is_constructible<DelEllipsis, int B::*>::value, "Error");
static_assert(!std::is_constructible<DelEllipsis, int D::*>::value, "Error");
static_assert(!std::is_constructible<DelEllipsis, Abstract>::value, "Error");
static_assert(!std::is_constructible<int, DelImplicitTo<int>>::value, "Error");
static_assert(!std::is_constructible<std::nullptr_t,
	      DelImplicitTo<std::nullptr_t>>::value, "Error");
static_assert(!std::is_constructible<int&,
	      DelImplicitTo<const int&>>::value, "Error");

static_assert(!std::is_constructible<int, void>::value, "Error");
static_assert(!std::is_constructible<void, int>::value, "Error");
static_assert(std::is_constructible<void*, int*>::value, "Error");
static_assert(!std::is_constructible<int*, void*>::value, "Error");
static_assert(std::is_constructible<int*, std::nullptr_t>::value, "Error");
static_assert(!std::is_constructible<std::nullptr_t, int*>::value, "Error");
static_assert(!std::is_constructible<Empty, E>::value, "Error");
static_assert(!std::is_constructible<Empty, SE>::value, "Error");
static_assert(!std::is_constructible<Empty, OpE>::value, "Error");
static_assert(!std::is_constructible<Empty, OpSE>::value, "Error");
static_assert(!std::is_constructible<Empty, void>::value, "Error");
static_assert(!std::is_constructible<Empty, void*>::value, "Error");
static_assert(!std::is_constructible<Empty, std::nullptr_t>::value, "Error");
static_assert(!std::is_constructible<Empty, int[]>::value, "Error");
static_assert(!std::is_constructible<Empty, int[3]>::value, "Error");
static_assert(!std::is_constructible<Empty, int>::value, "Error");
static_assert(!std::is_constructible<Abstract, int>::value, "Error");
static_assert(!std::is_constructible<Abstract, std::nullptr_t>::value, "Error");
static_assert(!std::is_constructible<std::nullptr_t, Abstract>::value, "Error");
static_assert(!std::is_constructible<Abstract, int[]>::value, "Error");
static_assert(std::is_constructible<B, D>::value, "Error");
#ifndef __cpp_aggregate_paren_init
static_assert(!std::is_constructible<D, B>::value, "Error");
#endif
static_assert(!std::is_constructible<int[], int[1]>::value, "Error");
static_assert(!std::is_constructible<int[1], int[]>::value, "Error");
static_assert(!std::is_constructible<int[], Empty>::value, "Error");
static_assert(!std::is_constructible<int[], std::nullptr_t>::value, "Error");
static_assert(!std::is_constructible<int[1], Abstract>::value, "Error");

static_assert(std::is_constructible<const int*, int*>::value, "Error");
static_assert(std::is_constructible<const void*, void*>::value, "Error");
static_assert(std::is_constructible<const void*, int*>::value, "Error");
static_assert(!std::is_constructible<int*, const void*>::value, "Error");

static_assert(std::is_constructible<int, E>::value, "Error");
static_assert(!std::is_constructible<E, int>::value, "Error");
static_assert(!std::is_constructible<E, E2>::value, "Error");
static_assert(std::is_constructible<E, E>::value, "Error");
static_assert(std::is_constructible<bool, E>::value, "Error");
static_assert(!std::is_constructible<E, bool>::value, "Error");
static_assert(std::is_constructible<double, E>::value, "Error");
static_assert(!std::is_constructible<E, double>::value, "Error");
static_assert(!std::is_constructible<std::nullptr_t, E>::value, "Error");
static_assert(!std::is_constructible<E, std::nullptr_t>::value, "Error");

static_assert(std::is_constructible<int, OpE>::value, "Error");
static_assert(!std::is_constructible<OpE, int>::value, "Error");
static_assert(!std::is_constructible<OpE, E2>::value, "Error");
static_assert(std::is_constructible<OpE, OpE>::value, "Error");
static_assert(std::is_constructible<bool, OpE>::value, "Error");
static_assert(!std::is_constructible<OpE, bool>::value, "Error");
static_assert(std::is_constructible<double, OpE>::value, "Error");
static_assert(!std::is_constructible<OpE, double>::value, "Error");
static_assert(!std::is_constructible<std::nullptr_t, OpE>::value, "Error");
static_assert(!std::is_constructible<OpE, std::nullptr_t>::value, "Error");

static_assert(!std::is_constructible<int, SE>::value, "Error");
static_assert(!std::is_constructible<SE, int>::value, "Error");
static_assert(!std::is_constructible<E, SE>::value, "Error");
static_assert(!std::is_constructible<SE, SE2>::value, "Error");
static_assert(std::is_constructible<SE, SE>::value, "Error");
static_assert(!std::is_constructible<bool, SE>::value, "Error");
static_assert(!std::is_constructible<SE, bool>::value, "Error");
static_assert(!std::is_constructible<double, SE>::value, "Error");
static_assert(!std::is_constructible<SE, double>::value, "Error");
static_assert(!std::is_constructible<std::nullptr_t, SE>::value, "Error");
static_assert(!std::is_constructible<SE, std::nullptr_t>::value, "Error");

static_assert(!std::is_constructible<int, OpSE>::value, "Error");
static_assert(!std::is_constructible<OpSE, int>::value, "Error");
static_assert(!std::is_constructible<OpE, OpSE>::value, "Error");
static_assert(!std::is_constructible<OpSE, SE2>::value, "Error");
static_assert(std::is_constructible<OpSE, OpSE>::value, "Error");
static_assert(!std::is_constructible<bool, OpSE>::value, "Error");
static_assert(!std::is_constructible<OpSE, bool>::value, "Error");
static_assert(!std::is_constructible<double, OpSE>::value, "Error");
static_assert(!std::is_constructible<OpSE, double>::value, "Error");
static_assert(!std::is_constructible<std::nullptr_t, OpSE>::value, "Error");
static_assert(!std::is_constructible<OpSE, std::nullptr_t>::value, "Error");

static_assert(!std::is_constructible<D*, B*>::value, "Error");
static_assert(!std::is_constructible<const volatile D*, B*>::value, "Error");
static_assert(!std::is_constructible<D*, const volatile B*>::value, "Error");

static_assert(!std::is_constructible<D*, B* const>::value, "Error");
static_assert(!std::is_constructible<const volatile D*, B* const>::value,
	      "Error");
static_assert(!std::is_constructible<D*, const volatile B* const>::value,
	      "Error");

static_assert(!std::is_constructible<D*, B*&>::value, "Error");
static_assert(!std::is_constructible<const volatile D*, B*&>::value, "Error");
static_assert(!std::is_constructible<D*, const volatile B*&>::value, "Error");

static_assert(!std::is_constructible<int B::*, int D::*>::value, "Error");
static_assert(!std::is_constructible<const volatile int B::*, int D::*>::value,
	      "Error");
static_assert(!std::is_constructible<int B::*, const volatile int D::*>::value,
	      "Error");

static_assert(!std::is_constructible<int B::*, int D::* const>::value, "Error");
static_assert(!std::is_constructible<const volatile int B::*,
	      int D::* const>::value, "Error");
static_assert(!std::is_constructible<int B::*,
	      const volatile int D::* const>::value, "Error");

static_assert(!std::is_constructible<int B::*, int D::*&>::value, "Error");
static_assert(!std::is_constructible<const volatile int B::*,
	      int D::*&>::value, "Error");
static_assert(!std::is_constructible<int B::*,
	      const volatile int D::*&>::value, "Error");

static_assert(!std::is_constructible<int B::*, int D::* const &>::value,
	      "Error");
static_assert(!std::is_constructible<const volatile int B::*,
	      int D::* const &>::value, "Error");
static_assert(!std::is_constructible<int B::*,
	      const volatile int D::* const &>::value, "Error");

static_assert(!std::is_constructible<int&&, int&>::value, "Error");
static_assert(!std::is_constructible<const int&&, int&>::value, "Error");

static_assert(std::is_constructible<B&, D&>::value, "Error");
static_assert(std::is_constructible<B&&, D&&>::value, "Error");
static_assert(std::is_constructible<const B&, D&>::value, "Error");
static_assert(std::is_constructible<const B&&, D&&>::value, "Error");
static_assert(!std::is_constructible<B&, const D&>::value, "Error");
static_assert(!std::is_constructible<B&&, const D&&>::value, "Error");

#if __cpp_aggregate_bases && __cpp_aggregate_paren_init
// In C++20 an rvalue reference or const lvalue reference can bind to a
// temporary of aggregate type that is initialized from a base class value.
constexpr bool v = true;
#else
constexpr bool v = false;
#endif

static_assert(!std::is_constructible<D&, B&>::value, "Error");
static_assert(v == std::is_constructible<D&&, B&&>::value, "Error");
static_assert(!std::is_constructible<D&, const B&>::value, "Error");
static_assert(v == std::is_constructible<D&&, const B&&>::value, "Error");
static_assert(v == std::is_constructible<const D&, B&>::value, "Error");
static_assert(v == std::is_constructible<const D&&, B&&>::value, "Error");

static_assert(!std::is_constructible<B&&, B&>::value, "Error");
static_assert(!std::is_constructible<B&&, D&>::value, "Error");
static_assert(std::is_constructible<B&&, ImplicitTo<D&&>>::value, "Error");
static_assert(std::is_constructible<B&&, ImplicitTo<D&&>&>::value, "Error");
static_assert(std::is_constructible<int&&, double&>::value, "Error");
static_assert(std::is_constructible<const int&,
	      ImplicitTo<int&>&>::value, "Error");
static_assert(std::is_constructible<const int&,
	      ImplicitTo<int&>>::value, "Error");
static_assert(std::is_constructible<const int&,
	      ExplicitTo<int&>&>::value, "Error");
static_assert(std::is_constructible<const int&,
	      ExplicitTo<int&>>::value, "Error");

static_assert(!std::is_constructible<B&&, ExplicitTo<D&&>>::value, "Error");
static_assert(!std::is_constructible<B&&, ExplicitTo<D&&>&>::value, "Error");

static_assert(!std::is_constructible<B&, B&&>::value, "Error");
static_assert(!std::is_constructible<D&, B&&>::value, "Error");
static_assert(!std::is_constructible<B&, D&&>::value, "Error");

static_assert(std::is_constructible<void(&)(), void(&)()>::value, "Error");
static_assert(std::is_constructible<void(&&)(), void(&&)()>::value, "Error");
static_assert(std::is_constructible<void(&&)(), void()>::value, "Error");

static_assert(!std::is_constructible<void>::value, "Error" );
static_assert(!std::is_constructible<void, int>::value, "Error" );
static_assert(!std::is_constructible<void, int, double>::value, "Error" );

static_assert(!std::is_constructible<int&>::value, "Error" );
static_assert(!std::is_constructible<const int&>::value, "Error" );
static_assert(!std::is_constructible<int&, int, int>::value, "Error" );
static_assert(!std::is_constructible<const int&, int, int>::value, "Error" );

static_assert(std::is_constructible<void(&)(), void()>::value, "Error");
static_assert(std::is_constructible<void(&)(), void(&&)()>::value, "Error");

static_assert(std::is_constructible<int&, int&>::value, "Error");
static_assert(!std::is_constructible<int&, const int&>::value, "Error");
static_assert(!std::is_constructible<int&, int>::value, "Error");
static_assert(!std::is_constructible<int&, int&&>::value, "Error");
static_assert(!std::is_constructible<int&, const int&&>::value, "Error");
static_assert(std::is_constructible<const int&, int&>::value, "Error");
static_assert(std::is_constructible<const int&, int>::value, "Error");
static_assert(std::is_constructible<const int&, const int>::value, "Error");
static_assert(std::is_constructible<const int&, int&&>::value, "Error");
static_assert(std::is_constructible<const int&, const int&&>::value, "Error");
static_assert(std::is_constructible<volatile int&, int&>::value, "Error");
static_assert(!std::is_constructible<volatile int&, const int&>::value,
	      "Error");
static_assert(!std::is_constructible<volatile int&, int>::value, "Error");
static_assert(!std::is_constructible<volatile int&, int&&>::value, "Error");
static_assert(!std::is_constructible<volatile int&, const int&&>::value,
	      "Error");
static_assert(std::is_constructible<const volatile int&, int&>::value, "Error");
static_assert(!std::is_constructible<const volatile int&, int>::value, "Error");
static_assert(!std::is_constructible<const volatile int&, const int>::value,
	      "Error");
static_assert(!std::is_constructible<const volatile int&, int&&>::value,
	      "Error");
static_assert(!std::is_constructible<const volatile int&, const int&&>::value,
	      "Error");

static_assert(std::is_constructible<int&&, int>::value, "Error");
static_assert(std::is_constructible<int&&, int&&>::value, "Error");
static_assert(!std::is_constructible<int&&, const int&&>::value, "Error");
static_assert(!std::is_constructible<int&&, int&>::value, "Error");
static_assert(!std::is_constructible<int&&, const int&>::value, "Error");
static_assert(std::is_constructible<int&&, double&>::value, "Error");
static_assert(std::is_constructible<const int&&, int>::value, "Error");
static_assert(std::is_constructible<const int&&, int&&>::value, "Error");
static_assert(std::is_constructible<const int&&, const int>::value, "Error");
static_assert(std::is_constructible<const int&&, const int&&>::value, "Error");
static_assert(!std::is_constructible<int&&, const int&>::value, "Error");
static_assert(!std::is_constructible<const int&&, int&>::value, "Error");
static_assert(!std::is_constructible<const int&&, const int&>::value, "Error");
static_assert(std::is_constructible<volatile int&&, int>::value, "Error");
static_assert(std::is_constructible<volatile int&&, int&&>::value, "Error");
static_assert(!std::is_constructible<volatile int&&, const int&&>::value,
	      "Error");
static_assert(!std::is_constructible<volatile int&&, int&>::value, "Error");
static_assert(!std::is_constructible<volatile int&&, const int&>::value,
	      "Error");
static_assert(std::is_constructible<volatile int&&, double&>::value, "Error");
static_assert(std::is_constructible<volatile const int&&, int>::value, "Error");
static_assert(std::is_constructible<const volatile int&&, int&&>::value,
	      "Error");
static_assert(std::is_constructible<const volatile int&&, const int>::value,
	      "Error");
static_assert(std::is_constructible<const volatile int&&, const int&&>::value,
	      "Error");
static_assert(!std::is_constructible<volatile int&&, const int&>::value,
	      "Error");
static_assert(!std::is_constructible<const volatile int&&, int&>::value,
	      "Error");
static_assert(!std::is_constructible<const volatile int&&,
	      const int&>::value, "Error");

static_assert(std::is_constructible<Empty&, Empty&>::value, "Error");
static_assert(!std::is_constructible<Empty&, const Empty&>::value, "Error");
static_assert(!std::is_constructible<Empty&, Empty>::value, "Error");
static_assert(!std::is_constructible<Empty&, Empty&&>::value, "Error");
static_assert(!std::is_constructible<Empty&, const Empty&&>::value, "Error");
static_assert(std::is_constructible<const Empty&, Empty&>::value, "Error");
static_assert(std::is_constructible<const Empty&, Empty>::value, "Error");
static_assert(std::is_constructible<const Empty&, const Empty>::value, "Error");
static_assert(std::is_constructible<const Empty&, Empty&&>::value, "Error");
static_assert(std::is_constructible<const Empty&, const Empty&&>::value,
	      "Error");
static_assert(std::is_constructible<volatile Empty&, Empty&>::value, "Error");
static_assert(!std::is_constructible<volatile Empty&, const Empty&>::value,
	      "Error");
static_assert(!std::is_constructible<volatile Empty&, Empty>::value, "Error");
static_assert(!std::is_constructible<volatile Empty&, Empty&&>::value, "Error");
static_assert(!std::is_constructible<volatile Empty&, const Empty&&>::value,
	      "Error");
static_assert(std::is_constructible<const volatile Empty&, Empty&>::value,
	      "Error");
static_assert(!std::is_constructible<const volatile Empty&, Empty>::value,
	      "Error");
static_assert(!std::is_constructible<const volatile Empty&,
	      const Empty>::value, "Error");
static_assert(!std::is_constructible<const volatile Empty&,
	      Empty&&>::value, "Error");
static_assert(!std::is_constructible<const volatile Empty&,
	      const Empty&&>::value, "Error");

static_assert(std::is_constructible<Empty&&, Empty>::value, "Error");
static_assert(std::is_constructible<Empty&&, Empty&&>::value, "Error");
static_assert(!std::is_constructible<Empty&&, const Empty&&>::value, "Error");
static_assert(!std::is_constructible<Empty&&, Empty&>::value, "Error");
static_assert(!std::is_constructible<Empty&&, const Empty&>::value, "Error");
static_assert(!std::is_constructible<Empty&&, double&>::value, "Error");
static_assert(!std::is_constructible<Empty&&, const double&>::value, "Error");
static_assert(std::is_constructible<const Empty&&, Empty>::value, "Error");
static_assert(std::is_constructible<const Empty&&, Empty&&>::value, "Error");
static_assert(std::is_constructible<const Empty&&, const Empty>::value,
	      "Error");
static_assert(std::is_constructible<const Empty&&, const Empty&&>::value,
	      "Error");
static_assert(!std::is_constructible<Empty&&, const Empty&>::value, "Error");
static_assert(!std::is_constructible<const Empty&&, Empty&>::value, "Error");
static_assert(!std::is_constructible<const Empty&&, const Empty&>::value,
	      "Error");
static_assert(std::is_constructible<volatile Empty&&, Empty>::value, "Error");
static_assert(std::is_constructible<volatile Empty&&, Empty&&>::value, "Error");
static_assert(!std::is_constructible<volatile Empty&&, const Empty&&>::value,
	      "Error");
static_assert(!std::is_constructible<volatile Empty&&, Empty&>::value, "Error");
static_assert(!std::is_constructible<volatile Empty&&, const Empty&>::value,
	      "Error");
static_assert(!std::is_constructible<volatile Empty&&, double&>::value,
	      "Error");
static_assert(!std::is_constructible<volatile Empty&&, const double&>::value,
	      "Error");
static_assert(std::is_constructible<const volatile Empty&&, Empty>::value,
	      "Error");
static_assert(std::is_constructible<const volatile Empty&&, Empty&&>::value,
	      "Error");
static_assert(std::is_constructible<const volatile Empty&&,
	      const Empty>::value, "Error");
static_assert(std::is_constructible<const volatile Empty&&,
	      const Empty&&>::value, "Error");
static_assert(!std::is_constructible<volatile Empty&&,
	      const Empty&>::value, "Error");
static_assert(!std::is_constructible<const volatile Empty&&,
	      Empty&>::value, "Error");
static_assert(!std::is_constructible<const volatile Empty&&,
	      const Empty&>::value, "Error");

static_assert(std::is_constructible<Ellipsis, int>::value, "Error");
static_assert(std::is_constructible<Ellipsis, Empty>::value, "Error");
static_assert(std::is_constructible<Ellipsis, std::nullptr_t>::value, "Error");
static_assert(std::is_constructible<Ellipsis, int[]>::value, "Error");
static_assert(std::is_constructible<Ellipsis, int[1]>::value, "Error");
static_assert(!std::is_constructible<Ellipsis, void>::value, "Error");

static_assert(std::is_constructible<int(&)[1], int(&)[1]>::value, "Error");
static_assert(std::is_constructible<const int(&)[1],
	      int(&)[1]>::value, "Error");
static_assert(std::is_constructible<volatile int(&)[1],
	      int(&)[1]>::value, "Error");
static_assert(std::is_constructible<const volatile int(&)[1],
	      int(&)[1]>::value, "Error");
static_assert(!std::is_constructible<int(&)[1],
	      const int(&)[1]>::value, "Error");
static_assert(!std::is_constructible<const int(&)[1],
	      volatile int(&)[1]>::value, "Error");

static_assert(std::is_constructible<int(&)[], int(&)[]>::value, "Error");

static_assert(!std::is_constructible<int(&)[1], int(&)[2]>::value, "Error");
static_assert(!std::is_constructible<int(&)[1], int&>::value, "Error");
static_assert(!std::is_constructible<int&, int(&)[1]>::value, "Error");

#ifndef __cpp_aggregate_paren_init
static_assert(!std::is_constructible<U, int>::value, "Error");
#endif
static_assert(!std::is_constructible<U, Empty>::value, "Error");

static_assert(!std::is_constructible<void(), void()>::value, "Error");
static_assert(!std::is_constructible<void(), int>::value, "Error");
static_assert(!std::is_constructible<void(), Abstract>::value, "Error");
static_assert(!std::is_constructible<void(), std::nullptr_t>::value, "Error");
static_assert(!std::is_constructible<void(), Empty>::value, "Error");
static_assert(!std::is_constructible<void(), U>::value, "Error");
static_assert(!std::is_constructible<void(), E>::value, "Error");
static_assert(!std::is_constructible<void(), SE>::value, "Error");
static_assert(!std::is_constructible<void(), OpE>::value, "Error");
static_assert(!std::is_constructible<void(), OpSE>::value, "Error");
static_assert(!std::is_constructible<void(), int[]>::value, "Error");
static_assert(!std::is_constructible<void(), int[1]>::value, "Error");

static_assert(!std::is_constructible<void() const,
	      void() volatile>::value, "Error");
static_assert(!std::is_constructible<void() const, int>::value, "Error");
static_assert(!std::is_constructible<void() const, Abstract>::value, "Error");
static_assert(!std::is_constructible<void() const, std::nullptr_t>::value,
	      "Error");
static_assert(!std::is_constructible<void() const, Empty>::value, "Error");
static_assert(!std::is_constructible<void() const, U>::value, "Error");
static_assert(!std::is_constructible<void() const, E>::value, "Error");
static_assert(!std::is_constructible<void() const, SE>::value, "Error");
static_assert(!std::is_constructible<void() const, OpE>::value, "Error");
static_assert(!std::is_constructible<void() const, OpSE>::value, "Error");
static_assert(!std::is_constructible<void() const, int[]>::value, "Error");
static_assert(!std::is_constructible<void() const, int[1]>::value, "Error");

static_assert(!std::is_constructible<void(int), void()>::value, "Error");
static_assert(!std::is_constructible<int, void()>::value, "Error");
static_assert(!std::is_constructible<Abstract, void()>::value, "Error");
static_assert(!std::is_constructible<std::nullptr_t, void()>::value, "Error");
static_assert(!std::is_constructible<Empty, void()>::value, "Error");
static_assert(!std::is_constructible<U, void()>::value, "Error");
static_assert(!std::is_constructible<E, void()>::value, "Error");
static_assert(!std::is_constructible<SE, void()>::value, "Error");
static_assert(!std::is_constructible<OpE, void()>::value, "Error");
static_assert(!std::is_constructible<OpSE, void()>::value, "Error");
static_assert(!std::is_constructible<int[], void()>::value, "Error");
static_assert(!std::is_constructible<int[1], void()>::value, "Error");

static_assert(!std::is_constructible<void(int) const,
      void() const>::value, "Error");
static_assert(!std::is_constructible<int, void() const>::value, "Error");
static_assert(!std::is_constructible<Abstract, void() const>::value, "Error");
static_assert(!std::is_constructible<std::nullptr_t, void() const>::value,
      "Error");
static_assert(!std::is_constructible<Empty, void() const>::value, "Error");
static_assert(!std::is_constructible<U, void() const>::value, "Error");
static_assert(!std::is_constructible<E, void() const>::value, "Error");
static_assert(!std::is_constructible<SE, void() const>::value, "Error");
static_assert(!std::is_constructible<OpE, void() const>::value, "Error");
static_assert(!std::is_constructible<OpSE, void() const>::value, "Error");
static_assert(!std::is_constructible<int[], void() const>::value, "Error");
static_assert(!std::is_constructible<int[1], void() const>::value, "Error");

static_assert(!std::is_constructible<void, int, int>::value, "Error");
static_assert(!std::is_constructible<void, Empty, B>::value, "Error");
static_assert(!std::is_constructible<void, Empty, Empty>::value, "Error");
static_assert(!std::is_constructible<void, U, Empty>::value, "Error");
static_assert(!std::is_constructible<void, U, U>::value, "Error");
static_assert(!std::is_constructible<void, std::nullptr_t,
	      std::nullptr_t>::value, "Error");
static_assert(!std::is_constructible<void, int[1], int[1]>::value, "Error");
static_assert(!std::is_constructible<void, int[], int[]>::value, "Error");
static_assert(!std::is_constructible<void, void, int>::value, "Error");
static_assert(!std::is_constructible<void, void, void>::value, "Error");
static_assert(!std::is_constructible<void, void(), void()>::value, "Error");
static_assert(!std::is_constructible<void, void() const,
	      void() volatile>::value, "Error");

static_assert(!std::is_constructible<int, int, int>::value, "Error");
static_assert(!std::is_constructible<const int, int, int>::value, "Error");
static_assert(!std::is_constructible<int, void, int>::value, "Error");
static_assert(!std::is_constructible<const int, void, int>::value, "Error");
static_assert(!std::is_constructible<int, void, void>::value, "Error");
static_assert(!std::is_constructible<const int, void, void>::value, "Error");
static_assert(!std::is_constructible<bool, int, int>::value, "Error");
static_assert(!std::is_constructible<const bool, int, int>::value, "Error");
static_assert(!std::is_constructible<std::nullptr_t, int, int>::value, "Error");
static_assert(!std::is_constructible<const std::nullptr_t, int, int>::value,
	      "Error");
static_assert(!std::is_constructible<std::nullptr_t, void, int>::value,
	      "Error");
static_assert(!std::is_constructible<const std::nullptr_t, void, int>::value,
	      "Error");
static_assert(!std::is_constructible<std::nullptr_t, void, void>::value,
	      "Error");
static_assert(!std::is_constructible<const std::nullptr_t, void, void>::value,
	      "Error");
static_assert(!std::is_constructible<E, int, int>::value, "Error");
static_assert(!std::is_constructible<const E, int, int>::value, "Error");
static_assert(!std::is_constructible<E, void, int>::value, "Error");
static_assert(!std::is_constructible<const E, void, int>::value, "Error");
static_assert(!std::is_constructible<E, void, void>::value, "Error");
static_assert(!std::is_constructible<const E, void, void>::value, "Error");
static_assert(!std::is_constructible<SE, int, int>::value, "Error");
static_assert(!std::is_constructible<const SE, int, int>::value, "Error");
static_assert(!std::is_constructible<SE, void, int>::value, "Error");
static_assert(!std::is_constructible<const SE, void, int>::value, "Error");
static_assert(!std::is_constructible<SE, void, void>::value, "Error");
static_assert(!std::is_constructible<const SE, void, void>::value, "Error");
static_assert(!std::is_constructible<OpE, int, int>::value, "Error");
static_assert(!std::is_constructible<const OpE, int, int>::value, "Error");
static_assert(!std::is_constructible<OpE, void, int>::value, "Error");
static_assert(!std::is_constructible<const OpE, void, int>::value, "Error");
static_assert(!std::is_constructible<OpE, void, void>::value, "Error");
static_assert(!std::is_constructible<const OpE, void, void>::value, "Error");
static_assert(!std::is_constructible<OpSE, int, int>::value, "Error");
static_assert(!std::is_constructible<const OpSE, int, int>::value, "Error");
static_assert(!std::is_constructible<OpSE, void, int>::value, "Error");
static_assert(!std::is_constructible<const OpSE, void, int>::value, "Error");
static_assert(!std::is_constructible<OpSE, void, void>::value, "Error");
static_assert(!std::is_constructible<const OpSE, void, void>::value, "Error");
static_assert(!std::is_constructible<Empty, int, int>::value, "Error");
static_assert(!std::is_constructible<const Empty, int, int>::value, "Error");
static_assert(!std::is_constructible<Empty, void, int>::value, "Error");
static_assert(!std::is_constructible<const Empty, void, int>::value, "Error");
static_assert(!std::is_constructible<Empty, void, void>::value, "Error");
static_assert(!std::is_constructible<const Empty, void, void>::value, "Error");
static_assert(!std::is_constructible<U, int, int>::value, "Error");
static_assert(!std::is_constructible<const U, int, int>::value, "Error");
static_assert(!std::is_constructible<U, void, int>::value, "Error");
static_assert(!std::is_constructible<const U, void, int>::value, "Error");
static_assert(!std::is_constructible<U, void, void>::value, "Error");
static_assert(!std::is_constructible<const U, void, void>::value, "Error");
static_assert(!std::is_constructible<B, int, int>::value, "Error");
static_assert(!std::is_constructible<const B, int, int>::value, "Error");
static_assert(!std::is_constructible<B, void, int>::value, "Error");
static_assert(!std::is_constructible<const B, void, int>::value, "Error");
static_assert(!std::is_constructible<B, void, void>::value, "Error");
static_assert(!std::is_constructible<const B, void, void>::value, "Error");
static_assert(!std::is_constructible<Any, int, int>::value, "Error");
static_assert(!std::is_constructible<const Any, int, int>::value, "Error");
static_assert(!std::is_constructible<Any, void, int>::value, "Error");
static_assert(!std::is_constructible<const Any, void, int>::value, "Error");
static_assert(!std::is_constructible<Any, void, void>::value, "Error");
static_assert(!std::is_constructible<const Any, void, void>::value, "Error");
static_assert(!std::is_constructible<nAny, void, int>::value, "Error");
static_assert(!std::is_constructible<const nAny, void, int>::value, "Error");
static_assert(!std::is_constructible<nAny, void, void>::value, "Error");
static_assert(!std::is_constructible<const nAny, void, void>::value, "Error");
static_assert(!std::is_constructible<FromArgs<>, int, int>::value, "Error");
static_assert(!std::is_constructible<const FromArgs<>, int, int>::value,
	      "Error");
static_assert(!std::is_constructible<FromArgs<>, void, int>::value, "Error");
static_assert(!std::is_constructible<const FromArgs<>, void, int>::value,
	      "Error");
static_assert(!std::is_constructible<FromArgs<>, void, void>::value, "Error");
static_assert(!std::is_constructible<const FromArgs<>, void, void>::value,
	      "Error");
static_assert(!std::is_constructible<Abstract, int, int>::value, "Error");
static_assert(!std::is_constructible<const Abstract, int, int>::value, "Error");
static_assert(!std::is_constructible<Abstract, void, int>::value, "Error");
static_assert(!std::is_constructible<const Abstract, void, int>::value,
	      "Error");
static_assert(!std::is_constructible<Abstract, void, void>::value, "Error");
static_assert(!std::is_constructible<const Abstract, void, void>::value,
	      "Error");
static_assert(!std::is_constructible<AbstractDelDtor, int, int>::value,
	      "Error");
static_assert(!std::is_constructible<const AbstractDelDtor, int, int>::value,
	      "Error");
static_assert(!std::is_constructible<AbstractDelDtor, void, int>::value,
	      "Error");
static_assert(!std::is_constructible<const AbstractDelDtor, void, int>::value,
	      "Error");
static_assert(!std::is_constructible<AbstractDelDtor, void, void>::value,
	      "Error");
static_assert(!std::is_constructible<const AbstractDelDtor, void, void>::value,
	      "Error");
static_assert(!std::is_constructible<int[1], int, int>::value, "Error");
static_assert(!std::is_constructible<const int[1], int, int>::value, "Error");
static_assert(!std::is_constructible<int[1], void, int>::value, "Error");
static_assert(!std::is_constructible<const int[1], void, int>::value, "Error");
static_assert(!std::is_constructible<int[1], void, void>::value, "Error");
static_assert(!std::is_constructible<const int[1], void, void>::value, "Error");
static_assert(!std::is_constructible<int&, int, int>::value, "Error");
static_assert(!std::is_constructible<int&, void, int>::value, "Error");
static_assert(!std::is_constructible<int&, void, void>::value, "Error");
static_assert(!std::is_constructible<int&, int&, int&>::value, "Error");
static_assert(!std::is_constructible<int&, void, int&>::value, "Error");
static_assert(!std::is_constructible<int&, void, void>::value, "Error");
static_assert(!std::is_constructible<std::nullptr_t&, int, int>::value,
	      "Error");
static_assert(!std::is_constructible<std::nullptr_t&, void, int>::value,
	      "Error");
static_assert(!std::is_constructible<std::nullptr_t&, void, void>::value,
	      "Error");
static_assert(!std::is_constructible<E&, int, int>::value, "Error");
static_assert(!std::is_constructible<E&, void, int>::value, "Error");
static_assert(!std::is_constructible<E&, void, void>::value, "Error");
static_assert(!std::is_constructible<SE&, int, int>::value, "Error");
static_assert(!std::is_constructible<SE&, void, int>::value, "Error");
static_assert(!std::is_constructible<SE&, void, void>::value, "Error");
static_assert(!std::is_constructible<OpE&, int, int>::value, "Error");
static_assert(!std::is_constructible<OpE&, void, int>::value, "Error");
static_assert(!std::is_constructible<OpE&, void, void>::value, "Error");
static_assert(!std::is_constructible<OpSE&, int, int>::value, "Error");
static_assert(!std::is_constructible<OpSE&, void, int>::value, "Error");
static_assert(!std::is_constructible<OpSE&, void, void>::value, "Error");
static_assert(!std::is_constructible<Empty&, int, int>::value, "Error");
static_assert(!std::is_constructible<Empty&, void, int>::value, "Error");
static_assert(!std::is_constructible<Empty&, void, void>::value, "Error");
static_assert(!std::is_constructible<U&, int, int>::value, "Error");
static_assert(!std::is_constructible<U&, void, int>::value, "Error");
static_assert(!std::is_constructible<U&, void, void>::value, "Error");
static_assert(!std::is_constructible<B&, int, int>::value, "Error");
static_assert(!std::is_constructible<B&, void, int>::value, "Error");
static_assert(!std::is_constructible<B&, void, void>::value, "Error");
static_assert(!std::is_constructible<Any&, int, int>::value, "Error");
static_assert(!std::is_constructible<Any&, void, int>::value, "Error");
static_assert(!std::is_constructible<Any&, void, void>::value, "Error");
static_assert(!std::is_constructible<nAny&, void, int>::value, "Error");
static_assert(!std::is_constructible<nAny&, void, void>::value, "Error");
static_assert(!std::is_constructible<FromArgs<>&, int, int>::value, "Error");
static_assert(!std::is_constructible<FromArgs<>&, void, int>::value, "Error");
static_assert(!std::is_constructible<FromArgs<>&, void, void>::value, "Error");
static_assert(!std::is_constructible<Abstract&, int, int>::value, "Error");
static_assert(!std::is_constructible<Abstract&, void, int>::value, "Error");
static_assert(!std::is_constructible<Abstract&, void, void>::value, "Error");
static_assert(!std::is_constructible<int(&)[1], int, int>::value, "Error");
static_assert(!std::is_constructible<int(&)[1], void, int>::value, "Error");
static_assert(!std::is_constructible<int(&)[1], void, void>::value, "Error");

static_assert(!std::is_constructible<void(), int, int>::value, "Error");
static_assert(!std::is_constructible<void(), void, int>::value, "Error");
static_assert(!std::is_constructible<void(), void, void>::value, "Error");
static_assert(!std::is_constructible<void(), void(), int>::value, "Error");
static_assert(!std::is_constructible<void(), void(), void()>::value, "Error");

static_assert(!std::is_constructible<void() const, int, int>::value, "Error");
static_assert(!std::is_constructible<void() const, void, int>::value, "Error");
static_assert(!std::is_constructible<void() const, void, void>::value, "Error");
static_assert(!std::is_constructible<void() const, void() volatile,
	      int>::value, "Error");
static_assert(!std::is_constructible<void() const, void() volatile const,
 	      void() const>::value, "Error");

static_assert(!std::is_constructible<FromArgs<int>, int, int>::value, "Error");
static_assert(!std::is_constructible<const FromArgs<int>, int, int>::value,
	      "Error");
static_assert(!std::is_constructible<FromArgs<int>, void, int>::value, "Error");
static_assert(!std::is_constructible<const FromArgs<int>, void, int>::value,
	      "Error");
static_assert(!std::is_constructible<FromArgs<int, int>, void, int>::value,
	      "Error");
static_assert(!std::is_constructible<const FromArgs<int, int>, void,
	      int>::value, "Error");

static_assert(!std::is_constructible<DelDtor, int, B, U>::value, "Error");
static_assert(!std::is_constructible<const DelDtor, int, B, U>::value, "Error");
static_assert(!std::is_constructible<DelDtor, int>::value, "Error");
static_assert(!std::is_constructible<const DelDtor, int>::value, "Error");
static_assert(!std::is_constructible<DelDtor>::value, "Error");
static_assert(!std::is_constructible<const DelDtor>::value, "Error");
static_assert(!std::is_constructible<DelDtor, void*, void(&)()>::value,
	      "Error");
static_assert(!std::is_constructible<const DelDtor, void*, void(&)()>::value,
	      "Error");

static_assert(!std::is_constructible<AbstractDelDtor>::value, "Error");
static_assert(!std::is_constructible<const AbstractDelDtor>::value, "Error");
static_assert(!std::is_constructible<DelEllipsis>::value, "Error");
static_assert(!std::is_constructible<const DelEllipsis>::value, "Error");
static_assert(!std::is_constructible<DelEllipsis, double>::value, "Error");
static_assert(!std::is_constructible<const DelEllipsis, double>::value,
	      "Error");
static_assert(!std::is_constructible<DelEllipsis, double, int&>::value,
	      "Error");
static_assert(!std::is_constructible<const DelEllipsis, double, int&>::value,
	      "Error");
static_assert(!std::is_constructible<DelnAny>::value, "Error");
static_assert(!std::is_constructible<const DelnAny>::value, "Error");
static_assert(!std::is_constructible<DelnAny, int>::value, "Error");
static_assert(!std::is_constructible<const DelnAny, int>::value, "Error");
static_assert(!std::is_constructible<DelnAny, int, void*>::value, "Error");
static_assert(!std::is_constructible<const DelnAny, int, void*>::value,
	      "Error");
static_assert(!std::is_constructible<DelnAny, Empty, B, D>::value, "Error");
static_assert(!std::is_constructible<const DelnAny, Empty, B, D>::value,
	      "Error");

// Deleted members in unions with non-trivial members:
static_assert(!std::is_constructible<NontrivialUnion>::value, "Error");
static_assert(!std::is_constructible<NontrivialUnion,
	      const NontrivialUnion&>::value, "Error");

// Unusual copy:
static_assert(!std::is_constructible<UnusualCopy>::value, "Error");
static_assert(!std::is_constructible<UnusualCopy, UnusualCopy>::value, "Error");
static_assert(!std::is_constructible<UnusualCopy,
	      UnusualCopy&&>::value, "Error");
static_assert(!std::is_constructible<UnusualCopy,
	      const UnusualCopy&>::value, "Error");
static_assert(std::is_constructible<UnusualCopy, UnusualCopy&>::value, "Error");

static_assert(std::is_constructible<FromArgs<int, char>,
	      int, char>::value, "Error");
static_assert(std::is_constructible<const FromArgs<int, char>,
	      int, char>::value, "Error");
static_assert(std::is_constructible<FromArgs<int, char>,
	      int, int>::value, "Error");
static_assert(std::is_constructible<const FromArgs<int, char>,
	      int, int>::value, "Error");
static_assert(std::is_constructible<nAny, int, int>::value, "Error");
static_assert(std::is_constructible<const nAny, int, int>::value, "Error");
static_assert(std::is_constructible<FromArgs<int, char>,
	      ImplicitTo<int>, ImplicitTo<char>>::value, "Error");
static_assert(std::is_constructible<const FromArgs<int, char>,
	      ImplicitTo<int>, ImplicitTo<char>>::value, "Error");
static_assert(std::is_constructible<Ellipsis, int, char>::value, "Error");
static_assert(std::is_constructible<const Ellipsis, int, char>::value, "Error");
static_assert(std::is_constructible<Ellipsis, B, U, int&>::value, "Error");
static_assert(std::is_constructible<const Ellipsis,
	      B, U, int&>::value, "Error");
static_assert(std::is_constructible<nAny, B, U, int&>::value, "Error");
static_assert(std::is_constructible<const nAny, B, U, int&>::value, "Error");
static_assert(std::is_constructible<FromArgs<std::initializer_list<int>,
	      std::initializer_list<B>>, std::initializer_list<int>,
	      std::initializer_list<B>>::value, "Error");
static_assert(std::is_constructible<const FromArgs<std::initializer_list<int>,
	      std::initializer_list<B>>, std::initializer_list<int>,
	      std::initializer_list<B>>::value, "Error");
static_assert(std::is_constructible<FromArgs<std::initializer_list<int>,
	      std::initializer_list<B>>, std::initializer_list<int>&,
	      std::initializer_list<B>&>::value, "Error");
static_assert(!std::is_constructible<FromArgs<std::initializer_list<int>&,
	      std::initializer_list<B>&>, std::initializer_list<int>,
	      std::initializer_list<B>>::value, "Error");

#if __cpp_aggregate_paren_init
// In C++20 arrays can be initialized using parentheses.
constexpr bool w = true;
#else
constexpr bool w = false;
#endif

static_assert(!std::is_constructible<FromArgs<std::initializer_list<int>>,
	      int, int>::value, "Error");
static_assert(!std::is_constructible<const
	      FromArgs<std::initializer_list<int>>, int, int>::value, "Error");
static_assert(w == std::is_constructible<B[2], B, B>::value, "Error");
static_assert(w == std::is_constructible<const B[2], B, B>::value, "Error");
static_assert(w == std::is_constructible<U[2], U, U>::value, "Error");
static_assert(w == std::is_constructible<const U[2], U, U>::value, "Error");

static_assert(!std::is_constructible<E, E, E>::value, "Error");
static_assert(!std::is_constructible<const E, E, E>::value, "Error");
static_assert(!std::is_constructible<SE, SE, SE>::value, "Error");
static_assert(!std::is_constructible<const SE, SE, SE>::value, "Error");
static_assert(!std::is_constructible<E, B, std::nullptr_t>::value, "Error");
static_assert(!std::is_constructible<const E, B, std::nullptr_t>::value,
	      "Error");
static_assert(!std::is_constructible<SE, B, std::nullptr_t>::value, "Error");
static_assert(!std::is_constructible<const SE, B, std::nullptr_t>::value,
	      "Error");
static_assert(!std::is_constructible<E, int[], int[]>::value, "Error");
static_assert(!std::is_constructible<const E, int[], int[]>::value, "Error");
static_assert(!std::is_constructible<SE, int[], int[]>::value, "Error");
static_assert(!std::is_constructible<const SE, int[], int[]>::value, "Error");

static_assert(!std::is_constructible<OpE, OpE, OpE>::value, "Error");
static_assert(!std::is_constructible<const OpE, OpE, OpE>::value, "Error");
static_assert(!std::is_constructible<OpSE, OpSE, OpSE>::value, "Error");
static_assert(!std::is_constructible<const OpSE, OpSE, OpSE>::value, "Error");
static_assert(!std::is_constructible<OpE, B, std::nullptr_t>::value, "Error");
static_assert(!std::is_constructible<const OpE, B, std::nullptr_t>::value,
	      "Error");
static_assert(!std::is_constructible<OpSE, B, std::nullptr_t>::value, "Error");
static_assert(!std::is_constructible<const OpSE, B, std::nullptr_t>::value,
	      "Error");
static_assert(!std::is_constructible<OpE, int[], int[]>::value, "Error");
static_assert(!std::is_constructible<const OpE, int[], int[]>::value, "Error");
static_assert(!std::is_constructible<OpSE, int[], int[]>::value, "Error");
static_assert(!std::is_constructible<const OpSE, int[], int[]>::value, "Error");

static_assert(!std::is_constructible<int[], int, int>::value, "Error");
static_assert(!std::is_constructible<const int[], int, int>::value, "Error");

static_assert(std::is_constructible<int&, ImplicitTo<int&>>::value, "Error");
static_assert(std::is_constructible<const int&, ImplicitTo<int&&>>::value,
	      "Error");
static_assert(std::is_constructible<int&&, ImplicitTo<int&&>>::value, "Error");
static_assert(std::is_constructible<const int&, ImplicitTo<int>>::value,
	      "Error");

static_assert(!std::is_constructible<const int&, ExplicitTo<int>>::value,
	      "Error");
static_assert(!std::is_constructible<int&&, ExplicitTo<int>>::value, "Error");

// Binding through reference-compatible type is required to perform
// direct-initialization as described in [over.match.ref] p. 1 b. 1:
static_assert(std::is_constructible<int&, ExplicitTo<int&>>::value, "Error");
static_assert(std::is_constructible<int&&, ExplicitTo<int&&>>::value, "Error");

// But an xvalue doesn't count for direct binding.
static_assert(!std::is_constructible<const int&, ExplicitTo<int&&>>::value,
	      "Error");

// Binding through temporary behaves like copy-initialization,
// see [dcl.init.ref] p. 5, very last sub-bullet:
static_assert(!std::is_constructible<const int&, ExplicitTo<double&&>>::value,
	      "Error");
static_assert(!std::is_constructible<int&&, ExplicitTo<double&&>>::value,
	      "Error");

static_assert(std::is_constructible<void(&&)(), void(&)()>::value, "Error");
