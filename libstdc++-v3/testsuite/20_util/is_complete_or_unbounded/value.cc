// { dg-do compile { target c++11 } }

// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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


struct incomplete_type;
class incomplete_type2;
union incomplete_union;
enum class incomplete_enum: int;
enum incomplete_enum2: int;
static_assert(!std::__is_complete_or_unbounded(std::__type_identity<incomplete_type>{}), "");
static_assert(!std::__is_complete_or_unbounded(std::__type_identity<incomplete_type2>{}), "");
static_assert(!std::__is_complete_or_unbounded(std::__type_identity<incomplete_union>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<incomplete_enum>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<incomplete_enum2>{}), "");

static_assert(!std::__is_complete_or_unbounded(std::__type_identity<incomplete_type[42]>{}), "");
static_assert(!std::__is_complete_or_unbounded(std::__type_identity<incomplete_type2[42]>{}), "");
static_assert(!std::__is_complete_or_unbounded(std::__type_identity<incomplete_union[42]>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<incomplete_enum[42]>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<incomplete_enum2[42]>{}), "");

static_assert(std::__is_complete_or_unbounded(std::__type_identity<incomplete_type[]>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<incomplete_type2[]>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<incomplete_union[]>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<incomplete_enum[]>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<incomplete_enum2[]>{}), "");


struct complete_type{ ~complete_type() = delete; };
class complete_type2{ int i; };
union complete_union{};
enum class complete_enum: int {};
enum complete_enum2: int {};
static_assert(std::__is_complete_or_unbounded(std::__type_identity<complete_type>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<complete_type2>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<complete_union>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<complete_enum>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<complete_enum2>{}), "");

static_assert(std::__is_complete_or_unbounded(std::__type_identity<complete_type[42]>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<complete_type2[42]>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<complete_union[42]>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<complete_enum[42]>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<complete_enum2[42]>{}), "");

static_assert(std::__is_complete_or_unbounded(std::__type_identity<complete_type[]>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<complete_type2[]>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<complete_union[]>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<complete_enum[]>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<complete_enum2[]>{}), "");


static_assert(std::__is_complete_or_unbounded(std::__type_identity<const complete_type>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<const complete_type2>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<const complete_union>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<const complete_enum>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<const complete_enum2>{}), "");

static_assert(std::__is_complete_or_unbounded(std::__type_identity<incomplete_type*>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<complete_type*>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<const incomplete_type*>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<const complete_type*>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<incomplete_type&>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<complete_type&>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<incomplete_type&&>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<complete_type&&>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<int complete_type::*>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<int (complete_type::*)(int)>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<int incomplete_type::*>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<int (incomplete_type::*)(int)>{}), "");

static_assert(std::__is_complete_or_unbounded(std::__type_identity<void(*)() noexcept>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<void(...) noexcept>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<void(&)(int)>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<void(*)()>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<void(incomplete_type)>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<void(&)()>{}), "");

static_assert(std::__is_complete_or_unbounded(std::__type_identity<std::nullptr_t>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<void>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<void*>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<const void* const>{}), "");
