// { dg-do compile { target c++11 } }

// Copyright (C) 2011-2016 Free Software Foundation, Inc.
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

using namespace __gnu_test::assign;

static_assert(std::is_assignable<int&, int>::value, "Error");
static_assert(std::is_assignable<int&, const int>::value, "Error");
static_assert(std::is_assignable<int&, int&>::value, "Error");
static_assert(std::is_assignable<int&, const int&>::value, "Error");

static_assert(!std::is_assignable<int, int>::value, "Error");
static_assert(!std::is_assignable<int, const int>::value, "Error");
static_assert(!std::is_assignable<int, int&>::value, "Error");
static_assert(!std::is_assignable<int, const int&>::value, "Error");

static_assert(!std::is_assignable<const int, int>::value, "Error");
static_assert(!std::is_assignable<const int, const int>::value, "Error");
static_assert(!std::is_assignable<const int, int&>::value, "Error");
static_assert(!std::is_assignable<const int, const int&>::value, "Error");

static_assert(!std::is_assignable<const int&, int>::value, "Error");
static_assert(!std::is_assignable<const int&, const int>::value, "Error");
static_assert(!std::is_assignable<const int&, int&>::value, "Error");
static_assert(!std::is_assignable<const int&, const int&>::value, "Error");

static_assert(std::is_assignable<Empty&, Empty>::value, "Error");
static_assert(std::is_assignable<Empty&, const Empty>::value, "Error");
static_assert(std::is_assignable<Empty&, Empty&>::value, "Error");
static_assert(std::is_assignable<Empty&, const Empty&>::value, "Error");
static_assert(std::is_assignable<Empty, Empty>::value, "Error");
static_assert(std::is_assignable<Empty, const Empty>::value, "Error");
static_assert(std::is_assignable<Empty, Empty&>::value, "Error");
static_assert(std::is_assignable<Empty, const Empty&>::value, "Error");

static_assert(std::is_assignable<B&, B>::value, "Error");
static_assert(std::is_assignable<B&, const B>::value, "Error");
static_assert(std::is_assignable<B&, B&>::value, "Error");
static_assert(std::is_assignable<B&, const B&>::value, "Error");
static_assert(std::is_assignable<B, B>::value, "Error");
static_assert(std::is_assignable<B, const B>::value, "Error");
static_assert(std::is_assignable<B, B&>::value, "Error");
static_assert(std::is_assignable<B, const B&>::value, "Error");

static_assert(std::is_assignable<bool&, bool>::value, "Error");
static_assert(std::is_assignable<bool&, const bool>::value, "Error");
static_assert(std::is_assignable<bool&, bool&>::value, "Error");
static_assert(std::is_assignable<bool&, const bool&>::value, "Error");

// bool is a fundamental type that does not allow assignment to an rvalue:
static_assert(!std::is_assignable<bool, bool>::value, "Error");
static_assert(!std::is_assignable<bool, const bool>::value, "Error");
static_assert(!std::is_assignable<bool, bool&>::value, "Error");
static_assert(!std::is_assignable<bool, const bool&>::value, "Error");

static_assert(std::is_assignable<std::nullptr_t&,
std::nullptr_t>::value, "Error");
static_assert(std::is_assignable<std::nullptr_t&, const
std::nullptr_t>::value, "Error");
static_assert(std::is_assignable<std::nullptr_t&,
std::nullptr_t&>::value, "Error");
static_assert(std::is_assignable<std::nullptr_t&, const
std::nullptr_t&>::value, "Error");

// std::nullptr_t is a fundamental type that does not allow
// assignment to an rvalue:
static_assert(!std::is_assignable<std::nullptr_t,
std::nullptr_t>::value, "Error");
static_assert(!std::is_assignable<std::nullptr_t, const
std::nullptr_t>::value, "Error");
static_assert(!std::is_assignable<std::nullptr_t,
std::nullptr_t&>::value, "Error");
static_assert(!std::is_assignable<std::nullptr_t, const
std::nullptr_t&>::value, "Error");

static_assert(std::is_assignable<E&, E>::value, "Error");
static_assert(std::is_assignable<E&, const E>::value, "Error");
static_assert(std::is_assignable<E&, E&>::value, "Error");
static_assert(std::is_assignable<E&, const E&>::value, "Error");

static_assert(std::is_assignable<int&, E>::value, "Error");
static_assert(std::is_assignable<int&, const E>::value, "Error");
static_assert(std::is_assignable<int&, E&>::value, "Error");
static_assert(std::is_assignable<int&, const E&>::value, "Error");

static_assert(!std::is_assignable<E&, int>::value, "Error");
static_assert(!std::is_assignable<E&, const int>::value, "Error");
static_assert(!std::is_assignable<E&, int&>::value, "Error");
static_assert(!std::is_assignable<E&, const int&>::value, "Error");

static_assert(!std::is_assignable<E&, E2>::value, "Error");
static_assert(!std::is_assignable<E&, const E2>::value, "Error");
static_assert(!std::is_assignable<E&, E2&>::value, "Error");
static_assert(!std::is_assignable<E&, const E2&>::value, "Error");

// E is not a class type and thus does not allow assignment to an rvalue:
static_assert(!std::is_assignable<E, E>::value, "Error");
static_assert(!std::is_assignable<E, const E>::value, "Error");
static_assert(!std::is_assignable<E, E&>::value, "Error");
static_assert(!std::is_assignable<E, const E&>::value, "Error");

static_assert(std::is_assignable<SE&, SE>::value, "Error");
static_assert(std::is_assignable<SE&, const SE>::value, "Error");
static_assert(std::is_assignable<SE&, SE&>::value, "Error");
static_assert(std::is_assignable<SE&, const SE&>::value, "Error");

static_assert(!std::is_assignable<int&, SE>::value, "Error");
static_assert(!std::is_assignable<int&, const SE>::value, "Error");
static_assert(!std::is_assignable<int&, SE&>::value, "Error");
static_assert(!std::is_assignable<int&, const SE&>::value, "Error");

static_assert(!std::is_assignable<SE&, int>::value, "Error");
static_assert(!std::is_assignable<SE&, const int>::value, "Error");
static_assert(!std::is_assignable<SE&, int&>::value, "Error");
static_assert(!std::is_assignable<SE&, const int&>::value, "Error");

// SE is not a class type and thus does not allow assignment to an rvalue:
static_assert(!std::is_assignable<SE, SE>::value, "Error");
static_assert(!std::is_assignable<SE, const SE>::value, "Error");
static_assert(!std::is_assignable<SE, SE&>::value, "Error");
static_assert(!std::is_assignable<SE, const SE&>::value, "Error");

static_assert(std::is_assignable<AnyAssign&, int>::value, "Error");
static_assert(std::is_assignable<AnyAssign&, std::nullptr_t>::value, "Error");
static_assert(std::is_assignable<AnyAssign&, E>::value, "Error");
static_assert(std::is_assignable<AnyAssign&, SE>::value, "Error");
static_assert(std::is_assignable<AnyAssign&, Empty>::value, "Error");
static_assert(std::is_assignable<AnyAssign&, U>::value, "Error");
static_assert(std::is_assignable<AnyAssign&, int&>::value, "Error");
static_assert(std::is_assignable<AnyAssign&, std::nullptr_t&>::value, "Error");
static_assert(std::is_assignable<AnyAssign&, E&>::value, "Error");
static_assert(std::is_assignable<AnyAssign&, SE&>::value, "Error");
static_assert(std::is_assignable<AnyAssign&, Empty&>::value, "Error");
static_assert(std::is_assignable<AnyAssign&, U&>::value, "Error");
static_assert(std::is_assignable<AnyAssign&, AnyAssign>::value, "Error");
static_assert(std::is_assignable<AnyAssign&,
std::initializer_list<int>>::value, "Error");

static_assert(std::is_assignable<AnyAssign&, int[1]>::value, "Error");
static_assert(std::is_assignable<AnyAssign&,
std::nullptr_t[1]>::value, "Error");
static_assert(std::is_assignable<AnyAssign&, E[1]>::value, "Error");
static_assert(std::is_assignable<AnyAssign&, SE[1]>::value, "Error");
static_assert(std::is_assignable<AnyAssign&, int(&)[1]>::value, "Error");
static_assert(std::is_assignable<AnyAssign&,
std::nullptr_t(&)[1]>::value, "Error");
static_assert(std::is_assignable<AnyAssign&, E(&)[1]>::value, "Error");
static_assert(std::is_assignable<AnyAssign&, SE(&)[1]>::value, "Error");

static_assert(std::is_assignable<int&, E>::value, "Error");
static_assert(!std::is_assignable<int&, SE>::value, "Error");
static_assert(std::is_assignable<bool&, E>::value, "Error");
static_assert(!std::is_assignable<bool&, SE>::value, "Error");
static_assert(std::is_assignable<bool&, void*>::value, "Error");
static_assert(std::is_assignable<bool&, int B::*>::value, "Error");
static_assert(std::is_assignable<bool&, void*>::value, "Error");
static_assert(!std::is_assignable<bool&, std::nullptr_t>::value, "Error");

static_assert(std::is_assignable<std::nullptr_t&,
std::nullptr_t>::value, "Error");
static_assert(std::is_assignable<void*&, std::nullptr_t>::value, "Error");
static_assert(std::is_assignable<int*&, std::nullptr_t>::value, "Error");
static_assert(std::is_assignable<int B::*&, std::nullptr_t>::value, "Error");
static_assert(!std::is_assignable<std::nullptr_t&, bool>::value, "Error");
static_assert(!std::is_assignable<void*&, bool>::value, "Error");
static_assert(!std::is_assignable<E&, bool>::value, "Error");
static_assert(!std::is_assignable<SE&, bool>::value, "Error");

static_assert(std::is_assignable<std::initializer_list<int>&,
std::initializer_list<int>>::value, "Error");
static_assert(std::is_assignable<std::initializer_list<int>&,
std::initializer_list<int>&&>::value, "Error");
static_assert(std::is_assignable<std::initializer_list<int>&, const
std::initializer_list<int>&&>::value, "Error");
static_assert(std::is_assignable<std::initializer_list<int>&,
std::initializer_list<int>&>::value, "Error");
static_assert(std::is_assignable<std::initializer_list<int>&, const
std::initializer_list<int>&>::value, "Error");
static_assert(!std::is_assignable<const std::initializer_list<int>&,
std::initializer_list<int>>::value, "Error");

static_assert(!std::is_assignable<const AnyAssign&, int>::value, "Error");
static_assert(!std::is_assignable<AnyAssign&, void>::value, "Error");

static_assert(!std::is_assignable<void, int>::value, "Error");
static_assert(!std::is_assignable<const void, int>::value, "Error");
static_assert(!std::is_assignable<int, void>::value, "Error");
static_assert(!std::is_assignable<int, const void>::value, "Error");
static_assert(!std::is_assignable<const int, void>::value, "Error");
static_assert(!std::is_assignable<const int, const void>::value, "Error");
static_assert(!std::is_assignable<int&, void>::value, "Error");
static_assert(!std::is_assignable<int&, const void>::value, "Error");
static_assert(!std::is_assignable<const int&, void>::value, "Error");
static_assert(!std::is_assignable<const int&, const void>::value, "Error");
static_assert(!std::is_assignable<void, void>::value, "Error");
static_assert(!std::is_assignable<const void, void>::value, "Error");
static_assert(!std::is_assignable<const void, const void>::value, "Error");

static_assert(!std::is_assignable<int[1], int[1]>::value, "Error");
static_assert(!std::is_assignable<int(&)[1], int[1]>::value, "Error");
static_assert(!std::is_assignable<int(&)[1], int(&)[1]>::value, "Error");
static_assert(!std::is_assignable<int[2], int[1]>::value, "Error");
static_assert(!std::is_assignable<int(&)[2], int[1]>::value, "Error");
static_assert(!std::is_assignable<int(&)[2], int(&)[1]>::value, "Error");
static_assert(!std::is_assignable<int[1], void>::value, "Error");
static_assert(!std::is_assignable<int(&)[1], void>::value, "Error");
static_assert(!std::is_assignable<void, int[1]>::value, "Error");
static_assert(!std::is_assignable<void, int(&)[1]>::value, "Error");

static_assert(!std::is_assignable<int[], int[]>::value, "Error");
static_assert(!std::is_assignable<int(&)[], int[]>::value, "Error");
static_assert(!std::is_assignable<int(&)[], int(&)[]>::value, "Error");
static_assert(!std::is_assignable<int[1], int[]>::value, "Error");
static_assert(!std::is_assignable<int(&)[1], int[]>::value, "Error");
static_assert(!std::is_assignable<int(&)[1], int(&)[]>::value, "Error");
static_assert(!std::is_assignable<int[], int[1]>::value, "Error");
static_assert(!std::is_assignable<int(&)[], int[1]>::value, "Error");
static_assert(!std::is_assignable<int(&)[], int(&)[1]>::value, "Error");
static_assert(!std::is_assignable<int[], void>::value, "Error");
static_assert(!std::is_assignable<int(&)[], void>::value, "Error");
static_assert(!std::is_assignable<void, int[]>::value, "Error");
static_assert(!std::is_assignable<void, int(&)[]>::value, "Error");

static_assert(std::is_assignable<DelCopyAssign&,
DelCopyAssign>::value, "Error");
static_assert(!std::is_assignable<DelCopyAssign&, const
DelCopyAssign>::value, "Error");
static_assert(!std::is_assignable<DelCopyAssign&,
DelCopyAssign&>::value, "Error");
static_assert(!std::is_assignable<DelCopyAssign&, const
DelCopyAssign&>::value, "Error");
static_assert(!std::is_assignable<DelCopyAssign&, void>::value, "Error");
static_assert(!std::is_assignable<DelCopyAssign&, void()>::value, "Error");
static_assert(!std::is_assignable<DelCopyAssign&, void(&)()>::value, "Error");
static_assert(!std::is_assignable<DelCopyAssign&, int>::value, "Error");

static_assert(std::is_assignable<DelAnyAssign&,
DelAnyAssign&&>::value, "Error");
static_assert(std::is_assignable<DelAnyAssign&, const
DelAnyAssign&>::value, "Error");
static_assert(std::is_assignable<DelAnyAssign,
DelAnyAssign&&>::value, "Error");
static_assert(std::is_assignable<DelAnyAssign, const
DelAnyAssign&>::value, "Error");

static_assert(!std::is_assignable<const DelAnyAssign&,
DelAnyAssign&&>::value, "Error");
static_assert(!std::is_assignable<const DelAnyAssign&, const
DelAnyAssign&>::value, "Error");
static_assert(!std::is_assignable<const DelAnyAssign,
DelAnyAssign&&>::value, "Error");
static_assert(!std::is_assignable<const DelAnyAssign, const
DelAnyAssign&>::value, "Error");

static_assert(!std::is_assignable<DelAnyAssign&, int>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, int&>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, const int&>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, void>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, void()>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, 
  void() const>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, void(&)()>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, void(&&)()>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&,
std::nullptr_t>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&,
std::nullptr_t&>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&,
std::initializer_list<int>>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&,
std::initializer_list<int>&>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, bool>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, bool&>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, E>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, E&>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, SE>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, SE&>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, Empty>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, Empty&>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, B>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, B&>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, U>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, U&>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, void*>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, int*>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, B*>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, D*>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, int B::*>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, int D::*>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, int[]>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, int[1]>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, int(&)[]>::value, "Error");
static_assert(!std::is_assignable<DelAnyAssign&, int(&)[1]>::value, "Error");

static_assert(!std::is_assignable<void(), void>::value, "Error");
static_assert(!std::is_assignable<void, void()>::value, "Error");
static_assert(!std::is_assignable<void(), void()>::value, "Error");

static_assert(!std::is_assignable<void(&)(), void>::value, "Error");
static_assert(!std::is_assignable<void, void(&)()>::value, "Error");
static_assert(!std::is_assignable<void(&)(), void(&)()>::value, "Error");
static_assert(!std::is_assignable<void(&)(), void()>::value, "Error");
static_assert(!std::is_assignable<void(), void(&)()>::value, "Error");

static_assert(std::is_assignable<int&, ImplicitTo<int>>::value, "Error");
static_assert(!std::is_assignable<int&, ExplicitTo<int>>::value, "Error");
static_assert(!std::is_assignable<int, ImplicitTo<int>>::value, "Error");
static_assert(!std::is_assignable<int, ExplicitTo<int>>::value, "Error");
static_assert(!std::is_assignable<const int, ImplicitTo<int>>::value, "Error");
static_assert(!std::is_assignable<const int, ExplicitTo<int>>::value, "Error");
static_assert(!std::is_assignable<const int&,
ImplicitTo<int>>::value, "Error");
static_assert(!std::is_assignable<const int&,
ExplicitTo<int>>::value, "Error");

static_assert(std::is_assignable<DelImplicitTo<int>&,
DelImplicitTo<int>>::value, "Error");
static_assert(std::is_assignable<DelImplicitTo<int>,
DelImplicitTo<int>>::value, "Error");
static_assert(!std::is_assignable<int&, DelImplicitTo<int>>::value, "Error");
static_assert(!std::is_assignable<int, DelImplicitTo<int>>::value, "Error");
static_assert(!std::is_assignable<const int&,
DelImplicitTo<int>>::value, "Error");
static_assert(!std::is_assignable<const int,
DelImplicitTo<int>>::value, "Error");
static_assert(!std::is_assignable<int&, DelExplicitTo<int>>::value, "Error");
static_assert(!std::is_assignable<int, DelExplicitTo<int>>::value, "Error");
static_assert(!std::is_assignable<const int&,
DelExplicitTo<int>>::value, "Error");
static_assert(!std::is_assignable<const int,
DelExplicitTo<int>>::value, "Error");

static_assert(std::is_assignable<B&, B>::value, "Error");
static_assert(std::is_assignable<B&, D>::value, "Error");
static_assert(std::is_assignable<B&, B&>::value, "Error");
static_assert(std::is_assignable<B&, D&>::value, "Error");
static_assert(!std::is_assignable<const B&, B&>::value, "Error");
static_assert(!std::is_assignable<const B&, D&>::value, "Error");
static_assert(!std::is_assignable<D&, B>::value, "Error");
static_assert(!std::is_assignable<D&, B&>::value, "Error");

static_assert(std::is_assignable<B*&, B*>::value, "Error");
static_assert(std::is_assignable<B*&, D*>::value, "Error");
static_assert(std::is_assignable<const B*&, D*>::value, "Error");
static_assert(std::is_assignable<const B*&, const D*>::value, "Error");
static_assert(std::is_assignable<B*&, B*&>::value, "Error");
static_assert(std::is_assignable<B*&, D*&>::value, "Error");
static_assert(std::is_assignable<const B*&, B*&>::value, "Error");
static_assert(std::is_assignable<const B*&, D*&>::value, "Error");
static_assert(!std::is_assignable<B* const&, B*&>::value, "Error");
static_assert(!std::is_assignable<B* const&, D*&>::value, "Error");
static_assert(!std::is_assignable<D*&, B*>::value, "Error");
static_assert(!std::is_assignable<D*&, B*&>::value, "Error");

static_assert(std::is_assignable<MO&, MO>::value, "Error");
static_assert(std::is_assignable<MO&, MO&&>::value, "Error");
static_assert(std::is_assignable<MO, MO>::value, "Error");
static_assert(std::is_assignable<MO, MO&&>::value, "Error");

static_assert(!std::is_assignable<const MO&, MO>::value, "Error");
static_assert(!std::is_assignable<const MO&, MO&&>::value, "Error");
static_assert(!std::is_assignable<MO&, const MO&&>::value, "Error");
static_assert(!std::is_assignable<MO&, MO&>::value, "Error");
static_assert(!std::is_assignable<MO&, const MO&>::value, "Error");
static_assert(!std::is_assignable<const MO, MO>::value, "Error");
static_assert(!std::is_assignable<const MO, MO&&>::value, "Error");
static_assert(!std::is_assignable<MO, const MO&&>::value, "Error");
static_assert(!std::is_assignable<MO, MO&>::value, "Error");
static_assert(!std::is_assignable<MO, const MO&>::value, "Error");

static_assert(!std::is_assignable<NontrivialUnion&,
NontrivialUnion>::value, "Error");
static_assert(!std::is_assignable<NontrivialUnion&,
NontrivialUnion&&>::value, "Error");
static_assert(!std::is_assignable<NontrivialUnion&,
NontrivialUnion&>::value, "Error");
static_assert(!std::is_assignable<NontrivialUnion&, const
NontrivialUnion&>::value, "Error");
static_assert(!std::is_assignable<NontrivialUnion&, const
NontrivialUnion&&>::value, "Error");

static_assert(std::is_assignable<Abstract&, Abstract>::value, "Error");
static_assert(std::is_assignable<Abstract&, Abstract&&>::value, "Error");
static_assert(std::is_assignable<Abstract&, Abstract&>::value, "Error");
static_assert(std::is_assignable<Abstract&, const Abstract&>::value, "Error");
static_assert(std::is_assignable<Abstract&, const Abstract&&>::value, "Error");
static_assert(std::is_assignable<Abstract&&, Abstract>::value, "Error");
static_assert(std::is_assignable<Abstract&&, Abstract&&>::value, "Error");
static_assert(std::is_assignable<Abstract&&, Abstract&>::value, "Error");
static_assert(std::is_assignable<Abstract&&, const Abstract&>::value, "Error");
static_assert(std::is_assignable<Abstract&&, const
Abstract&&>::value, "Error");

static_assert(std::is_assignable<AbstractDelDtor&,
AbstractDelDtor>::value, "Error");
static_assert(std::is_assignable<AbstractDelDtor&,
AbstractDelDtor&&>::value, "Error");
static_assert(std::is_assignable<AbstractDelDtor&,
AbstractDelDtor&>::value, "Error");
static_assert(std::is_assignable<AbstractDelDtor&, const
AbstractDelDtor&>::value, "Error");
static_assert(std::is_assignable<AbstractDelDtor&, const
AbstractDelDtor&&>::value, "Error");
static_assert(std::is_assignable<AbstractDelDtor&&,
AbstractDelDtor>::value, "Error");
static_assert(std::is_assignable<AbstractDelDtor&&,
AbstractDelDtor&&>::value, "Error");
static_assert(std::is_assignable<AbstractDelDtor&&,
AbstractDelDtor&>::value, "Error");
static_assert(std::is_assignable<AbstractDelDtor&&, const
AbstractDelDtor&>::value, "Error");
static_assert(std::is_assignable<AbstractDelDtor&&, const
AbstractDelDtor&&>::value, "Error");

static_assert(std::is_assignable<DelDef&, DelDef>::value, "Error");
static_assert(std::is_assignable<DelDef&, DelDef&&>::value, "Error");
static_assert(std::is_assignable<DelDef&, DelDef&>::value, "Error");
static_assert(std::is_assignable<DelDef&, const DelDef&>::value, "Error");
static_assert(std::is_assignable<DelDef&, const DelDef&&>::value, "Error");
static_assert(std::is_assignable<DelDef&&, DelDef>::value, "Error");
static_assert(std::is_assignable<DelDef&&, DelDef&&>::value, "Error");
static_assert(std::is_assignable<DelDef&&, DelDef&>::value, "Error");
static_assert(std::is_assignable<DelDef&&, const DelDef&>::value, "Error");
static_assert(std::is_assignable<DelDef&&, const DelDef&&>::value, "Error");

static_assert(std::is_assignable<Ellipsis&, Ellipsis>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, const Ellipsis>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, Ellipsis&>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, const Ellipsis&>::value, "Error");
static_assert(std::is_assignable<Ellipsis, Ellipsis>::value, "Error");
static_assert(std::is_assignable<Ellipsis, const Ellipsis>::value, "Error");
static_assert(std::is_assignable<Ellipsis, Ellipsis&>::value, "Error");
static_assert(std::is_assignable<Ellipsis, const Ellipsis&>::value, "Error");

static_assert(!std::is_assignable<Ellipsis&, void>::value, "Error");

static_assert(std::is_assignable<Ellipsis&, int>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, const int>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, int&>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, const int&>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, Empty>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, const Empty>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, Empty&>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, const Empty&>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, E>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, const E>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, E&>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, const E&>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, SE>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, const SE>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, SE&>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, const SE&>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, bool>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, const bool>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, bool&>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, const bool&>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, std::nullptr_t>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, const
std::nullptr_t>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, std::nullptr_t&>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, const
std::nullptr_t&>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, void*>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, const void*>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, void*&>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, const void*&>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, void()>::value, "Error");
static_assert(std::is_assignable<Ellipsis&, void(&)()>::value, "Error");

static_assert(std::is_assignable<DelEllipsis&, DelEllipsis>::value, "Error");
static_assert(std::is_assignable<DelEllipsis&, const
DelEllipsis>::value, "Error");
static_assert(std::is_assignable<DelEllipsis&, DelEllipsis&>::value, "Error");
static_assert(std::is_assignable<DelEllipsis&, const
DelEllipsis&>::value, "Error");
static_assert(std::is_assignable<DelEllipsis, DelEllipsis>::value, "Error");
static_assert(std::is_assignable<DelEllipsis, const
DelEllipsis>::value, "Error");
static_assert(std::is_assignable<DelEllipsis, DelEllipsis&>::value, "Error");
static_assert(std::is_assignable<DelEllipsis, const
DelEllipsis&>::value, "Error");

static_assert(!std::is_assignable<DelEllipsis&, void>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, int>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, const int>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, int&>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, const int&>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, Empty>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, const Empty>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, Empty&>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, const Empty&>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, E>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, const E>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, E&>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, const E&>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, SE>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, const SE>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, SE&>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, const SE&>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, bool>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, const bool>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, bool&>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, const bool&>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&,
std::nullptr_t>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, const
std::nullptr_t>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&,
std::nullptr_t&>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, const
std::nullptr_t&>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, void*>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, const void*>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, void*&>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, const void*&>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, void()>::value, "Error");
static_assert(!std::is_assignable<DelEllipsis&, void(&)()>::value, "Error");

static_assert(std::is_assignable<FromArgs<int>&, int>::value, "Error");
static_assert(std::is_assignable<FromArgs<int>&, const int>::value, "Error");
static_assert(!std::is_assignable<FromArgs<int>&,
ImplicitTo<int>>::value, "Error");
static_assert(!std::is_assignable<FromArgs<int>&, ImplicitTo<const
int>>::value, "Error");
static_assert(!std::is_assignable<FromArgs<int>&,
ExplicitTo<int>>::value, "Error");
static_assert(!std::is_assignable<FromArgs<int>&, ExplicitTo<const
int>>::value, "Error");

static_assert(!std::is_assignable<DelFromArgs<int>&, int>::value, "Error");
static_assert(!std::is_assignable<DelFromArgs<int>&, const
int>::value, "Error");

static_assert(std::is_assignable<void(*&)(),
ImplicitTo<void(*)()>>::value, "Error");
static_assert(!std::is_assignable<void(*&)(),
ExplicitTo<void(*)()>>::value, "Error");

static_assert(std::is_assignable<UAssignAll&, UAssignAll>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, const
UAssignAll>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, UAssignAll&>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, const
UAssignAll&>::value, "Error");

static_assert(std::is_assignable<UAssignAll, UAssignAll>::value, "Error");
static_assert(std::is_assignable<UAssignAll, const
UAssignAll>::value, "Error");
static_assert(std::is_assignable<UAssignAll, UAssignAll&>::value, "Error");
static_assert(std::is_assignable<UAssignAll, const
UAssignAll&>::value, "Error");

static_assert(!std::is_assignable<UAssignAll&, void>::value, "Error");
static_assert(!std::is_assignable<const UAssignAll&, void>::value, "Error");
static_assert(!std::is_assignable<const UAssignAll&,
UAssignAll>::value, "Error");
static_assert(!std::is_assignable<const UAssignAll&, const
UAssignAll>::value, "Error");
static_assert(!std::is_assignable<const UAssignAll&,
UAssignAll&>::value, "Error");
static_assert(!std::is_assignable<const UAssignAll&, const
UAssignAll&>::value, "Error");
static_assert(!std::is_assignable<UAssignAll&, void() const>::value, "Error");
static_assert(!std::is_assignable<UAssignAll&, void() &>::value, "Error");
static_assert(!std::is_assignable<UAssignAll&, void() const volatile &&>::value, "Error");

static_assert(std::is_assignable<UAssignAll&, int>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, int&>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, E>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, E&>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, SE>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, SE&>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, double>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, double&>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, Empty>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, Empty&>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, B>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, B&>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, U>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, U&>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, std::nullptr_t>::value, "Error");
static_assert(std::is_assignable<UAssignAll&,
std::nullptr_t&>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, void()>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, void(&)()>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, void(*)()>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, void(*&)()>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, int*>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, int*&>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, void*>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, void*&>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, const int*>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, const int*&>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, const void*>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, const void*&>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, int[1]>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, int(&)[1]>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, int[]>::value, "Error");
static_assert(std::is_assignable<UAssignAll&, int(&)[]>::value, "Error");

static_assert(!std::is_assignable<UDelAssignAll&, int>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, int&>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, E>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, E&>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, SE>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, SE&>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, double>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, double&>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, Empty>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, Empty&>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, B>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, B&>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, U>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, U&>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&,
std::nullptr_t>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&,
std::nullptr_t&>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, void()>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, void(&)()>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, void()
 const>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, void(*)()>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, void(*&)()>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, int*>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, int*&>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, void*>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, void*&>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, const int*>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, const
int*&>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, const
void*>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, const
void*&>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, int[1]>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, int(&)[1]>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, int[]>::value, "Error");
static_assert(!std::is_assignable<UDelAssignAll&, int(&)[]>::value, "Error");

static_assert(!std::is_assignable<void(&)(), std::nullptr_t>::value, "Error");
static_assert(!std::is_assignable<std::nullptr_t, void(&)()>::value, "Error");
static_assert(!std::is_assignable<void(&)(), int[]>::value, "Error");
static_assert(!std::is_assignable<int[], void(&)()>::value, "Error");
static_assert(!std::is_assignable<int[], std::nullptr_t>::value, "Error");
static_assert(!std::is_assignable<std::nullptr_t, int[]>::value, "Error");
static_assert(!std::is_assignable<int[1], std::nullptr_t>::value, "Error");
static_assert(!std::is_assignable<std::nullptr_t, int[1]>::value, "Error");
static_assert(!std::is_assignable<void, std::nullptr_t>::value, "Error");
static_assert(!std::is_assignable<std::nullptr_t, void>::value, "Error");
static_assert(!std::is_assignable<const D&, B&>::value, "Error");
static_assert(!std::is_assignable<const B&, B&>::value, "Error");

static_assert(std::is_assignable<B&, const D&>::value, "Error");
static_assert(std::is_assignable<B&, const B&>::value, "Error");
static_assert(std::is_assignable<int&, const int&>::value, "Error");
static_assert(std::is_assignable<int&, const double&>::value, "Error");
