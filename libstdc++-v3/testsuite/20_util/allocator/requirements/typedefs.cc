// { dg-do compile { target c++11 } }
// { dg-require-effective-target hosted }

// Copyright (C) 2012-2025 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.


#include <memory>
#include <type_traits>

// Check std::allocator for required typedefs.

using std::is_same;
using std::allocator;

static_assert( is_same<allocator<int>::size_type, std::size_t>::value,
               "size_type" );
static_assert( is_same<allocator<int>::difference_type, std::ptrdiff_t>::value,
               "difference_type" );
#if __cplusplus <= 201703L
static_assert( is_same<allocator<int>::pointer, int*>::value,
               "pointer" );
static_assert( is_same<allocator<int>::const_pointer, const int*>::value,
               "const_pointer" );
static_assert( is_same<allocator<int>::reference, int&>::value,
               "reference" );
static_assert( is_same<allocator<int>::const_reference, const int&>::value,
               "const_reference" );
#endif
static_assert( is_same<allocator<int>::value_type, int>::value,
               "value_type" );

#if __cplusplus <= 201703L
static_assert( is_same<allocator<int>::rebind<char>::other,
                       allocator<char>>::value,
               "rebind::other" );
#endif

static_assert( is_same<allocator<int>::propagate_on_container_move_assignment,
                       std::true_type>::value,
               "propagate_on_container_move_assignment" );

#if __cplusplus <= 202302L
using IAE = allocator<int>::is_always_equal; // { dg-warning "deprecated" "" { target { c++20_only || c++23_only } } }
static_assert( is_same<IAE, std::true_type>::value, "is_always_equal" );
#else
struct B { using is_always_equal = int; };
struct tester : B, std::allocator<int> { is_always_equal unambig; };
#endif

// Test required typedefs for allocator<void> specialization.
static_assert( is_same<allocator<void>::value_type, void>::value,
	       "void value_type" );
#if __cplusplus <= 201703L
static_assert( is_same<allocator<void>::pointer, void*>::value,
	       "void pointer" );
static_assert( is_same<allocator<void>::const_pointer, const void*>::value,
	       "void const_pointer" );
static_assert( is_same<allocator<void>::rebind<char>::other,
                       allocator<char>>::value,
               "void rebind::other" );
#else
// Since C++20 allocator<void> uses the primary template, so has the same types.
static_assert( is_same<allocator<void>::propagate_on_container_move_assignment,
                       std::true_type>::value,
               "propagate_on_container_move_assignment" );

#if __cplusplus <= 202302L
using VIAE = allocator<void>::is_always_equal; // { dg-warning "deprecated" "" { target { c++20_only || c++23_only } } }
static_assert( is_same<VIAE, std::true_type>::value, "is_always_equal" );
#else
struct tester2 : B, std::allocator<void> { is_always_equal unambig; };
#endif
#endif
