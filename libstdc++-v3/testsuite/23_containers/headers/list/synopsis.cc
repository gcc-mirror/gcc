// { dg-do compile }
// { dg-require-normal-mode "" }
// { dg-require-normal-namespace "" }

// Copyright (C) 2007-2023 Free Software Foundation, Inc.
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

#include <list>

#if __cplusplus >= 201103L
# define NOTHROW(X) noexcept(X)
#else
# define NOTHROW(X)
#endif

namespace std {
_GLIBCXX_BEGIN_NAMESPACE_CXX11
  template <class T, class Allocator> class list;
_GLIBCXX_END_NAMESPACE_CXX11

  template <class T, class Allocator>
  bool operator==(const list<T,Allocator>& x, const list<T,Allocator>&);

  template <class T, class Allocator>
  bool operator< (const list<T,Allocator>& x, const list<T,Allocator>&);

  template <class T, class Allocator>
    bool operator!=(const list<T,Allocator>& x, const list<T,Allocator>&);

  template <class T, class Allocator>
    bool operator> (const list<T,Allocator>& x, const list<T,Allocator>&);

  template <class T, class Allocator>
    bool operator>=(const list<T,Allocator>& x, const list<T,Allocator>&);

  template <class T, class Allocator>
    bool operator<=(const list<T,Allocator>& x, const list<T,Allocator>&);

  template <class T, class Allocator>
    void swap(list<T,Allocator>& x, list<T,Allocator>& y)
      NOTHROW(noexcept(x.swap(y)));
}
