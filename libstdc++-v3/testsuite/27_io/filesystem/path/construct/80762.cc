// Copyright (C) 2018-2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <filesystem>

using std::filesystem::path;

// PR libstdc++/80762.cc
static_assert( !std::is_constructible_v<path, void> );
static_assert( !std::is_constructible_v<path, volatile path> );
static_assert( !std::is_constructible_v<path, volatile path&> );
static_assert( !std::is_constructible_v<path, const volatile path> );
static_assert( !std::is_constructible_v<path, const volatile path&> );

// PR libstdc++/90454.cc
static_assert( !std::is_constructible_v<path, void*> );
static_assert( !std::is_constructible_v<path, const void*> );
static_assert( !std::is_constructible_v<path, volatile void*> );
static_assert( !std::is_constructible_v<path, const volatile void*> );
static_assert( !std::is_constructible_v<path, void*&> );
static_assert( !std::is_constructible_v<path, void* const&> );
static_assert( !std::is_constructible_v<path, const void* const&> );
