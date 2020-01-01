// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

// PR libstdc++/86734
// LWG 1052. reverse_iterator::operator-> should also support smart pointers
// LWG 2775. reverse_iterator is does not compile for fancy pointers

#include <iterator>
#include <testsuite_hooks.h>

void
test01()
{
  // Example 1 from LWG 1052

  struct X { int m; };

  static X x;

  struct IterX {
    typedef std::bidirectional_iterator_tag iterator_category;
    typedef X& reference;
    struct pointer
    {
      pointer(X& v) : value(v) {}
      X& value;
      X* operator->() const {return &value;}
    };
    typedef std::ptrdiff_t difference_type;
    typedef X value_type;
    // additional iterator requirements not important for this issue

    reference operator*() const { return x; }
    pointer operator->() const { return pointer(x); }
    IterX& operator--() {return *this;}

  };

  std::reverse_iterator<IterX> ix;
  VERIFY( &ix->m == &(*ix).m );
}

void
test02()
{
  // Example 2 from LWG 1052

  struct P {
    P() : first(10), second(20.0) { }
    int first;
    double second;
  };
  P op;
  std::reverse_iterator<P*> ri(&op + 1);
  VERIFY( ri->first == 10 );
}

// N.B. Example 3 from LWG 1052 isn't expected to work,
// because a caching iterator like IterX is not a forward iterator.

int
main()
{
  test01();
  test02();
}
