// { dg-options "-std=gnu++17" }
// { dg-do compile }

// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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
#include "../../../../include/std/pmroptional"

using std::pmr::optional;

#include <testsuite_hooks.h>

struct my_alloc{} test_alloc;

struct value_type1
{
};
namespace std {
template<>
struct __domain_allocator_traits<value_type1, std::pmr::polymorphic_allocator<void>> : std::true_type {
    static my_alloc*  convert(std::pmr::polymorphic_allocator<void> _a) { return &test_alloc; }
};
}
struct value_type2
{
	static my_alloc*  domain_alloc_convert(std::pmr::polymorphic_allocator<void> _a) { return &test_alloc; }
};

struct value_type3
{
	typedef std::pmr::polymorphic_allocator<void> allocator_type;
};
static_assert(std::uses_allocator<value_type1, std::pmr::polymorphic_allocator<void>>{}, "");
static_assert(std::uses_allocator<value_type2, std::pmr::polymorphic_allocator<void>>{}, "");
static_assert(std::uses_allocator<value_type3, std::pmr::polymorphic_allocator<void>>{}, "");


