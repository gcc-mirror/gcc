// { dg-do compile }
// 2001-06-21  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 24.4.1.2 Reverse iterators

#include <iterator>

void test01()
{
  using namespace std;

  // Check for required base class.
  long l;
  typedef reverse_iterator<long*> test_iterator;
  typedef iterator<iterator_traits<long*>::iterator_category,
		   iterator_traits<long*>::value_type,
		   iterator_traits<long*>::difference_type,
		   iterator_traits<long*>::pointer,
                   iterator_traits<long*>::reference>
    base_iterator;
  test_iterator  r_it(&l);
  base_iterator* base __attribute__((unused)) = &r_it;

  // Check for required typedefs
  typedef test_iterator::value_type value_type;
  typedef test_iterator::difference_type difference_type;
  typedef test_iterator::pointer pointer;
  typedef test_iterator::reference reference;
  typedef test_iterator::iterator_category iteratory_category;
}

// Make sure iterator can be instantiated.
template class std::reverse_iterator<int*>;
