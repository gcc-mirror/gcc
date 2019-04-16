// Copyright (C) 2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

// P0357R3 reference_wrapper for incomplete types

#include <functional>

struct Incomplete;

template class std::reference_wrapper<Incomplete>;

Incomplete& f();

std::reference_wrapper<Incomplete> r = f();
static_assert( std::is_same_v<decltype(r)::type, Incomplete> );
static_assert( std::is_same_v<decltype(r.get()), Incomplete&> );

std::reference_wrapper r2 = f();
static_assert( std::is_same_v<decltype(r), decltype(r2)> );
