// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// Copyright (C) 2010-2014 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <utility>

template <class T>
  struct C
  {
    T t_;

    C() { }

    template <class U,
              class = typename std::enable_if
                    <
                        !std::is_lvalue_reference<U>::value
                    >::type>
      C(U&& u) : t_(std::forward<T>(std::move(u).get())) { }

    C(C&& c) : t_(std::forward<T>(c.t_)) { }
  };

template <class T>
  struct Derived
  : C<T>
  {
    Derived() { }
    Derived(Derived&& d) : C<T>(std::forward<C<T>>(d)) { }
  };

// http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2009/n2951.html
// Test E.
int main()
{
  Derived<int> d;
  Derived<int> d2(std::move(d));
}
