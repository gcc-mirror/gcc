// Copyright (C) 2001, 2004, 2005 Free Software Foundation, Inc.
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

// 23.2.2.1 list constructors, copy, and assignment

#include <list>
#include <testsuite_hooks.h>

bool test __attribute__((unused)) = true;

// A nontrivial type.
template<typename T>
  struct A { };

// Another nontrivial type
struct B { };

// A nontrivial type convertible from an int
struct C {
  C(int i) : i_(i) { }
  bool operator==(const C& rhs) { return i_ == rhs.i_; }
  int i_;
};

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
void
test01()
{
  std::list< A<B> > list0101;
  VERIFY(list0101.begin() == list0101.end());
  VERIFY(list0101.size() == 0);

  // check type definitions -- will fail compile if missing
  typedef std::list< A<B> >::reference              reference;
  typedef std::list< A<B> >::const_reference        const_reference;
  typedef std::list< A<B> >::iterator               iterator;
  typedef std::list< A<B> >::const_iterator         const_iterator;
  typedef std::list< A<B> >::size_type              size_type;
  typedef std::list< A<B> >::difference_type        difference_type;
  typedef std::list< A<B> >::value_type             value_type;
  typedef std::list< A<B> >::allocator_type         allocator_type;
  typedef std::list< A<B> >::pointer                pointer;
  typedef std::list< A<B> >::const_pointer          const_pointer;
  typedef std::list< A<B> >::reverse_iterator       reverse_iterator;
  typedef std::list< A<B> >::const_reverse_iterator const_reverse_iterator;

  // allocator checks?
}

int main()
{
  test01();
  return 0;
}
