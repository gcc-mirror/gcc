// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

// PR libstdc++/93983

// LWG 3420.
// cpp17-iterator should check that the type looks like an iterator first

#include <filesystem>
#include <iterator>
#include <concepts>

struct Foo
{
  Foo(const std::filesystem::path& p);
};

static_assert(std::copyable<Foo>);

struct X
{
  template<typename T, typename = std::iterator_traits<T>::iterator_category>
    X(const T&);
};

static_assert(std::copyable<X>);
