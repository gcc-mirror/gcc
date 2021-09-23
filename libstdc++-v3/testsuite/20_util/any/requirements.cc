// { dg-do compile { target c++17 } }

// Copyright (C) 2016-2021 Free Software Foundation, Inc.
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

#include <any>
#include <testsuite_hooks.h>

#include <memory>

using std::any;
using std::unique_ptr;

static_assert(std::is_assignable<any&, int>::value);
static_assert(!std::is_assignable<any&, unique_ptr<int>>::value);
static_assert(std::is_constructible<any, int>::value);
static_assert(!std::is_constructible<any, unique_ptr<int>>::value);
static_assert(!std::is_assignable<any&, const unique_ptr<int>&>::value);
static_assert(!std::is_constructible<any&, const unique_ptr<int>&>::value);
static_assert(!std::is_assignable<any&, unique_ptr<int>&>::value);
static_assert(!std::is_constructible<any&, unique_ptr<int>&>::value);

struct NoDefaultCtor
{
  NoDefaultCtor() = delete;
};

static_assert(!std::is_constructible<any,
	      std::in_place_type_t<NoDefaultCtor>>::value);

static_assert(!std::is_constructible<any,
	      std::in_place_type_t<NoDefaultCtor>&>::value);

static_assert(!std::is_constructible<any,
	      std::in_place_type_t<NoDefaultCtor>&&>::value);

static_assert(!std::is_constructible<any,
	      const std::in_place_type_t<NoDefaultCtor>&>::value);

static_assert(!std::is_constructible<any,
	      const std::in_place_type_t<NoDefaultCtor>&&>::value);
