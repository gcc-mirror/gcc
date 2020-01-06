// { dg-do compile }

// Copyright (C) 2005-2020 Free Software Foundation, Inc.
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

// TR1 2.2.2 Template class shared_ptr [tr.util.smartptr.shared]

// { dg-options "-std=c++98" }

#include <tr1/memory>
#include <testsuite_hooks.h>

struct A { };
std::auto_ptr<A> source() { return std::auto_ptr<A>(); }

// 2.2.3.3 shared_ptr assignment [tr.util.smartptr.shared.assign]

// Assignment from rvalue auto_ptr
int
test01()
{
  std::tr1::shared_ptr<A> a;
  a = source(); // { dg-error "" }

  return 0;
}

int
main()
{
  test01();
  return 0;
}
