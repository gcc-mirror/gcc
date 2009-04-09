// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// 2008-08-27  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2008, 2009 Free Software Foundation
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

#include <unordered_set>

void test01()
{
  // Check for required typedefs
  typedef std::unordered_multiset<int>            test_type;

  typedef test_type::key_type                     key_type;
  typedef test_type::value_type                   value_type;
  typedef test_type::hasher                       hasher;
  typedef test_type::key_equal                    key_equal;
  typedef test_type::allocator_type               allocator_type;
  typedef test_type::pointer                      pointer;
  typedef test_type::const_pointer                const_pointer;
  typedef test_type::reference                    reference;
  typedef test_type::const_reference              const_reference;
  typedef test_type::size_type                    size_type;
  typedef test_type::difference_type              difference_type;
  typedef test_type::iterator                     iterator;
  typedef test_type::const_iterator               const_iterator;
  typedef test_type::local_iterator               local_iterator;
  typedef test_type::const_local_iterator         const_local_iterator;
}
