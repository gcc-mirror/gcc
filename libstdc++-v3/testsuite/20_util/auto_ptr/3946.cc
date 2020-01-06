// Copyright (C) 2000-2020 Free Software Foundation, Inc.
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

// 20.4.5 Template class auto_ptr [lib.auto.ptr]

// { dg-options "-std=c++98" }

#include <memory>
#include <testsuite_hooks.h>

// libstdc++/3946
// http://gcc.gnu.org/ml/libstdc++/2002-07/msg00024.html
struct Base { };
struct Derived : public Base { };

std::auto_ptr<Derived> 
conversiontest08() { return std::auto_ptr<Derived>(new Derived); }

void
test08()
{
  std::auto_ptr<Base> ptr;
  ptr = conversiontest08();
}


int 
main()
{
  test08();
  return 0;
}
