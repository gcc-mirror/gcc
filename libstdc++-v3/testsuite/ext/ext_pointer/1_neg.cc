// Bob Walters 10-2008

// Test for Container using non-standard pointer types.

// Copyright (C) 2008-2023 Free Software Foundation, Inc.
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


// { dg-do compile }

#include <algorithm>
#include <testsuite_hooks.h>
#include <ext/pointer.h>

using __gnu_cxx::_Pointer_adapter;
using __gnu_cxx::_Relative_pointer_impl;
using __gnu_cxx::__static_pointer_cast;
using __gnu_cxx::__const_pointer_cast;


struct A {
  int i;
};
struct B : public A{
  int j;
};
typedef _Pointer_adapter<_Relative_pointer_impl<B> > B_pointer; 
typedef _Pointer_adapter<_Relative_pointer_impl<const B> > const_B_pointer; 
typedef _Pointer_adapter<_Relative_pointer_impl<A> > A_pointer; 
typedef _Pointer_adapter<_Relative_pointer_impl<const A> > const_A_pointer; 


void test01(void) {
  A a;
  B b;

  A_pointer aptr( &a );

  // Can't implicitly cast from A* to B*
  B_pointer bptr1(aptr); // { dg-error "required from here" 31 }
  B_pointer bptr2(&a); // { dg-error "required from here" 32 }

  // but explicit cast/conversion is OK.
  B_pointer bptr3(__static_pointer_cast<B_pointer>(aptr)); // ok
  B_pointer bptr4(__static_pointer_cast<B_pointer>(&a)); // ok

  // Can't implicitly cast from A* to B*
  bptr1 = aptr; // { dg-error "required from here" 39 }
  bptr1 = &a; // { dg-error "required from here" 40 }

  // but explicit cast/conversion is OK.
  bptr1 = __static_pointer_cast<B_pointer>(aptr); // ok
  bptr1 = __static_pointer_cast<B_pointer>(&a); // ok

  // Similarly, can't shed constness via implicit cast
  const_A_pointer captr(&a);
  A_pointer aptr2(captr); // { dg-error "required from here" 48 }

  // but explicit cast/conversion is OK.
  A_pointer aptr3(__const_pointer_cast<A_pointer>(captr)); // ok

  // Similarly, can't shed constness via implicit cast
  aptr2 = captr; // { dg-error "required from here" 54 }

  // but explicit cast/conversion is OK.
  aptr3 = __const_pointer_cast<A_pointer>(captr); // ok

  // Combine explicit const cast with implicit downcast.
  const_B_pointer cbptr(&b);
  A_pointer aptr4(cbptr); // { dg-error "required from here" 61 }
  aptr4 = cbptr; // { dg-error "required from here" 62 }

  A_pointer aptr5(__const_pointer_cast<B_pointer>(cbptr)); // ok
  aptr5 = __const_pointer_cast<B_pointer>(cbptr);  // ok
}

// { dg-prune-output "include" }
