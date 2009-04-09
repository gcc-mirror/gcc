// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// 2007-07-10  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2007, 2009 Free Software Foundation, Inc.
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

#include <memory>
#include <utility>

template<class T, class A1, class A2>
  std::shared_ptr<T>
  factory(A1&& a1, A2&& a2)
  {
    return std::shared_ptr<T>(new T(std::forward<A1>(a1),
				    std::forward<A2>(a2))); // { dg-error "no matching function" }
  }

struct A
{
  A(int&, const double&);
};

void g()
{
  std::shared_ptr<A> sp1 = factory<A>(2, 1.414); // { dg-error "instantiated from here" }
}

// { dg-excess-errors "" }
