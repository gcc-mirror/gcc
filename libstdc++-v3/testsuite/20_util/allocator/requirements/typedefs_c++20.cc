// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#include <memory>

template<typename Alloc>
  concept has_pointer = requires { typename Alloc::pointer; };

template<typename Alloc>
  concept has_const_pointer = requires { typename Alloc::const_pointer; };

template<typename Alloc>
  concept has_reference = requires { typename Alloc::reference; };

template<typename Alloc>
  concept has_const_reference = requires { typename Alloc::const_reference; };

template<typename Alloc>
  concept has_rebind = requires { typename Alloc::template rebind<long>; };

template<typename Alloc>
  concept has_construct = requires(Alloc& a, int* p) { a.construct(p); };

template<typename Alloc>
  concept has_destroy = requires(Alloc& a, int* p) { a.destroy(p); };

template<typename Alloc>
  concept has_max_size = requires(Alloc& a) { a.max_size(); };

using A = std::allocator<int>;

static_assert( !has_pointer<A> );
static_assert( !has_const_pointer<A> );
static_assert( !has_reference<A> );
static_assert( !has_const_reference<A> );
static_assert( !has_rebind<A> );
static_assert( !has_construct<A> );
static_assert( !has_destroy<A> );
static_assert( !has_max_size<A> );

using V = std::allocator<void>;

static_assert( !has_pointer<V> );
static_assert( !has_const_pointer<V> );
static_assert( !has_reference<V> );
static_assert( !has_const_reference<V> );
static_assert( !has_rebind<V> );
static_assert( !has_construct<V> );
static_assert( !has_destroy<V> );
static_assert( !has_max_size<V> );
