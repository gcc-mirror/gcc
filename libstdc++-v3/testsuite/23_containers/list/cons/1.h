// Copyright (C) 2001-2025 Free Software Foundation, Inc.
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

// 23.2.2.1 list constructors, copy, and assignment

#include <testsuite_hooks.h>

// A nontrivial type.
template<typename T>
  struct A { };

// Another nontrivial type
struct B { };

// Default constructor, basic properties
//
// This test verifies the following.
// 23.2.2.1     explicit list(const a& = Allocator())
// 23.1 (7)     iterator behaviour of empty containers
// 23.2.2       iterator begin()
// 23.2.2       iterator end()
// 23.2.2       size_type size() const
// 23.2.2	existence of required typedefs
//
template<typename _Tp>
void
cons01()
{
  typedef _Tp list_type;

  list_type list0101;
  VERIFY(list0101.begin() == list0101.end());
  VERIFY(list0101.size() == 0);

  // check type definitions -- will fail compile if missing
  typedef typename list_type::reference              reference;
  typedef typename list_type::const_reference        const_reference;
  typedef typename list_type::iterator               iterator;
  typedef typename list_type::const_iterator         const_iterator;
  typedef typename list_type::size_type              size_type;
  typedef typename list_type::difference_type        difference_type;
  typedef typename list_type::value_type             value_type;
  typedef typename list_type::allocator_type         allocator_type;
  typedef typename list_type::pointer                pointer;
  typedef typename list_type::const_pointer          const_pointer;
  typedef typename list_type::reverse_iterator       reverse_iterator;
  typedef typename list_type::const_reverse_iterator const_reverse_iterator;
}
