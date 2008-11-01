// { dg-options "-std=gnu++0x " }
// { dg-do compile }

// Copyright (C) 2008 Free Software Foundation
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

// 20.6.6.3 Template class weak_ptr [util.smartptr.weak]

#include <memory>
#include <testsuite_hooks.h>

struct A { };

// 20.8.13.3.6 weak_ptr comparison [util.smartptr.weak.cmp] (removed)

int
test01()
{
    std::weak_ptr<A> p1;
    // { dg-excess-errors "deleted function" }
    p1 < p1;  // { dg-error "used here" }
    return 0;
}

int 
main()
{
  test01();
  return 0;
}
