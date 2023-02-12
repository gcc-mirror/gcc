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

#include <vector>

#if __cplusplus >= 201103L
# define NOTHROW(X) noexcept(X)
#else
# define NOTHROW(X)
#endif

#if __cplusplus >= 202002L
# define CONSTEXPR constexpr
#else
# define CONSTEXPR
#endif

namespace std {
  template <class T, class Allocator> class vector;

  template <class T, class Allocator>
    CONSTEXPR
    bool operator==(const vector<T,Allocator>& x,
                    const vector<T,Allocator>& y);

#if __cplusplus < 202002L
  template <class T, class Allocator>
    bool operator< (const vector<T,Allocator>& x,
                    const vector<T,Allocator>& y);

  template <class T, class Allocator>
    bool operator!=(const vector<T,Allocator>& x,
                    const vector<T,Allocator>& y);

  template <class T, class Allocator>
    bool operator> (const vector<T,Allocator>& x,
                    const vector<T,Allocator>& y);

  template <class T, class Allocator>
    bool operator>=(const vector<T,Allocator>& x,
                    const vector<T,Allocator>& y);

  template <class T, class Allocator>
    bool operator<=(const vector<T,Allocator>& x,
                    const vector<T,Allocator>& y);
#endif

  template <class T, class Allocator>
    CONSTEXPR
    void swap(vector<T,Allocator>& x, vector<T,Allocator>& y)
      NOTHROW(noexcept(x.swap(y)));

  template <class Allocator> class vector<bool,Allocator>;

  template <class Allocator>
    CONSTEXPR
    bool operator==(const vector<bool,Allocator>& x,
                    const vector<bool,Allocator>& y);

#if __cplusplus < 202002L
  template <class Allocator>
    bool operator< (const vector<bool,Allocator>& x,
                    const vector<bool,Allocator>& y);

  template <class Allocator>
    bool operator!=(const vector<bool,Allocator>& x,
                    const vector<bool,Allocator>& y);

  template <class Allocator>
    bool operator> (const vector<bool,Allocator>& x,
                    const vector<bool,Allocator>& y);

  template <class Allocator>
    bool operator>=(const vector<bool,Allocator>& x,
                    const vector<bool,Allocator>& y);

  template <class Allocator>
    bool operator<=(const vector<bool,Allocator>& x,
                    const vector<bool,Allocator>& y);
#endif

  template <class Allocator>
    CONSTEXPR
    void swap(vector<bool,Allocator>& x, vector<bool,Allocator>& y);
}
