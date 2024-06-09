// Copyright (C) 2017-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17_only } }

#include <functional>

// PR libstdc++/83427

int f(short) noexcept { return 0; }
std::reference_wrapper<decltype(f)> r(f);
static_assert(std::is_same_v<decltype(r)::result_type, int>);
static_assert(std::is_same_v<decltype(r)::argument_type, short>);

auto* p = &f;
std::reference_wrapper<decltype(&f)> r2(p);
static_assert(std::is_same_v<decltype(r2)::result_type, int>);
static_assert(std::is_same_v<decltype(r2)::argument_type, short>);

struct X { long f() const & noexcept { return 0L; } };
auto m = &X::f;
std::reference_wrapper<decltype(m)> r3(m);
static_assert(std::is_same_v<decltype(r3)::result_type, long>);
static_assert(std::is_same_v<decltype(r3)::argument_type, const X*>);
