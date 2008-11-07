// Bob Walters 10-2008

// Test for Container using non-standard pointer types.

// Copyright (C) 2008
// Free Software Foundation, Inc.
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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

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
  bool test __attribute__((unused)) = true;

  A a;
  B b;

  A_pointer aptr( &a );

  // Can't implicitly cast from A* to B*
  B_pointer bptr1(aptr); // { dg-error "instantiated from here" 31 }
  B_pointer bptr2(&a); // { dg-error "instantiated from here" 32 }

  // but explicit cast/conversion is OK.
  B_pointer bptr3(__static_pointer_cast<B_pointer>(aptr)); // ok
  B_pointer bptr4(__static_pointer_cast<B_pointer>(&a)); // ok

  // Can't implicitly cast from A* to B*
  bptr1 = aptr; // { dg-error "instantiated from here" 39 }
  bptr1 = &a; // { dg-error "instantiated from here" 40 }

  // but explicit cast/conversion is OK.
  bptr1 = __static_pointer_cast<B_pointer>(aptr); // ok
  bptr1 = __static_pointer_cast<B_pointer>(&a); // ok

  // Similarly, can't shed constness via implicit cast
  const_A_pointer captr(&a);
  A_pointer aptr2(captr); // { dg-error "instantiated from here" 48 }

  // but explicit cast/conversion is OK.
  A_pointer aptr3(__const_pointer_cast<A_pointer>(captr)); // ok

  // Similarly, can't shed constness via implicit cast
  aptr2 = captr; // { dg-error "instantiated from here" 54 }

  // but explicit cast/conversion is OK.
  aptr3 = __const_pointer_cast<A_pointer>(captr); // ok

  // Combine explicit const cast with implicit downcast.
  const_B_pointer cbptr(&b);
  A_pointer aptr4(cbptr); // { dg-error "instantiated from here" 61 }
  aptr4 = cbptr; // { dg-error "instantiated from here" 62 }

  A_pointer aptr5(__const_pointer_cast<B_pointer>(cbptr)); // ok
  aptr5 = __const_pointer_cast<B_pointer>(cbptr);  // ok
}

// { dg-error "invalid conversion " "" { target *-*-* } 334 }
// { dg-error "initializing argument 1 of" "" { target *-*-* } 334 }
// { dg-error "invalid conversion " "" { target *-*-* } 324 }
// { dg-error "initializing argument 1 of" "" { target *-*-* } 324 }
// { dg-error "invalid conversion " "" { target *-*-* } 352 }
// { dg-error "initializing argument 1 of" "" { target *-*-* } 352 }
// { dg-error "invalid conversion " "" { target *-*-* } 360 }
// { dg-error "initializing argument 1 of" "" { target *-*-* } 360 }
// { dg-excess-errors "In constructor" }

