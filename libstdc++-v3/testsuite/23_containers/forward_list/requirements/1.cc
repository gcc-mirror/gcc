// { dg-options "-std=gnu++0x" }

// Copyright (C) 2008, 2009 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without Pred the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 23.2.3.n forward_list xxx [lib.forward_list.xxx]

#include <forward_list>
#include <testsuite_hooks.h>

bool test __attribute__((unused)) = true;

// A nontrivial type.
template<typename T>
  struct A { };

// Another nontrivial type
struct B { };

// A nontrivial type convertible from an int
struct C
{
  C(int i) : i_(i) { }
  bool operator==(const C& rhs) { return i_ == rhs.i_; }
  int i_;
};

// This test verifies the following.
//
void
test01()
{
  std::forward_list< A<B> > lst;
  VERIFY(lst.begin() == lst.end());
  VERIFY(std::distance(lst.begin(), lst.end()) == 0);

  // check type definitions -- will fail compile if missing
  typedef std::forward_list< A<B> >::reference              reference;
  typedef std::forward_list< A<B> >::const_reference        const_reference;
  typedef std::forward_list< A<B> >::iterator               iterator;
  typedef std::forward_list< A<B> >::const_iterator         const_iterator;
  typedef std::forward_list< A<B> >::size_type              size_type;
  typedef std::forward_list< A<B> >::difference_type        difference_type;
  typedef std::forward_list< A<B> >::value_type             value_type;
  typedef std::forward_list< A<B> >::allocator_type         allocator_type;
  typedef std::forward_list< A<B> >::pointer                pointer;
  typedef std::forward_list< A<B> >::const_pointer          const_pointer;
}

int
main()
{
  test01();
  return 0;
}
