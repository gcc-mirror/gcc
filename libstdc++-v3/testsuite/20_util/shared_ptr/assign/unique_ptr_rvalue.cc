// { dg-do compile { target c++11 } }

// Copyright (C) 2008-2017 Free Software Foundation, Inc.
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

// 20.7.12.2 Template class shared_ptr [util.smartptr.shared]

#include <memory>

struct A { };
std::unique_ptr<A> source() { return std::unique_ptr<A>(); }

// 20.7.12.2.3 shared_ptr assignment [util.smartptr.shared.assign]

// Assignment from rvalue unique_ptr
void
test01()
{
  std::shared_ptr<A> a;
  a = source();
}

void
test02()
{
  std::shared_ptr<A> a;
  std::unique_ptr<A> u;
  a = std::move(u);
}

int 
main()
{
  test01();
  test02();
  return 0;
}
