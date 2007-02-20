// { dg-do compile }

// Copyright (C) 2007 Free Software Foundation, Inc.
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

#include <deque>

namespace std {
  template <class T, class Allocator> class deque;

  template <class T, class Allocator>
    bool operator== (const deque<T,Allocator>& x, const deque<T,Allocator>& y);

  template <class T, class Allocator>
    bool operator< (const deque<T,Allocator>& x, const deque<T,Allocator>& y);

  template <class T, class Allocator>
    bool operator!= (const deque<T,Allocator>& x, const deque<T,Allocator>& y);

  template <class T, class Allocator>
    bool operator> (const deque<T,Allocator>& x, const deque<T,Allocator>& y);

  template <class T, class Allocator>
    bool operator>= (const deque<T,Allocator>& x, const deque<T,Allocator>& y);

  template <class T, class Allocator>
    bool operator<= (const deque<T,Allocator>& x, const deque<T,Allocator>& y);

  template <class T, class Allocator>
    void swap(deque<T,Allocator>& x, deque<T,Allocator>& y);
}
