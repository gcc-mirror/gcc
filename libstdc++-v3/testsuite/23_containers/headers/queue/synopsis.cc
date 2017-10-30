// { dg-do compile }
// { dg-require-normal-namespace "" }

// Copyright (C) 2007-2017 Free Software Foundation, Inc.
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

#include <queue>

namespace std {
  template <class T, class Container> class queue;
  template <class T, class Container>
    bool operator==(const queue<T, Container>& x,
                    const queue<T, Container>& y);

  template <class T, class Container>
    bool operator< (const queue<T, Container>& x,
                    const queue<T, Container>& y);

  template <class T, class Container>
    bool operator!=(const queue<T, Container>& x,
                    const queue<T, Container>& y);

  template <class T, class Container>
    bool operator> (const queue<T, Container>& x,
                    const queue<T, Container>& y);

  template <class T, class Container>
    bool operator>=(const queue<T, Container>& x,
                    const queue<T, Container>& y);

  template <class T, class Container>
    bool operator<=(const queue<T, Container>& x,
                    const queue<T, Container>& y);

  template <class T, class Container, class Compare>
  class priority_queue;
}
