// Test for Container using non-standard pointer types.

// Copyright (C) 2008-2016 Free Software Foundation, Inc.
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


#include <algorithm>
#include <testsuite_hooks.h>
#include <ext/cast.h>
#include <ext/pointer.h>

using __gnu_cxx::_Pointer_adapter;
using __gnu_cxx::_Relative_pointer_impl;
using __gnu_cxx::__static_pointer_cast;
using __gnu_cxx::__const_pointer_cast;


void 
test01() {
  typedef _Pointer_adapter<_Relative_pointer_impl<int> >       pointer;
  typedef _Pointer_adapter<_Relative_pointer_impl<const int> > const_pointer;

  int A[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

  // basic pointer assignment/access tests.
  pointer x = &A[0];
  VERIFY(*x == 0);
  VERIFY(std::equal(x, x+10, A)); 
  pointer y(&A[9]);
  VERIFY(*y == 9);

  // assignability
  pointer z(x);
  VERIFY(z==x);
  VERIFY(*z == 0);

  z = y;
  VERIFY(z==y);
  VERIFY(z!=x);
  VERIFY(z>x);
  VERIFY(*z == 9);
  
  // pointer arithmetic
  VERIFY(*++x == 1);
  VERIFY(*--x == 0);
  VERIFY(*(x++) == 0);
  VERIFY(*(x--) == 1);
  VERIFY(*(x+2) == 2);
  VERIFY(*(2+x) == 2);
  VERIFY(*(y-2) == 7);
  VERIFY(y - x == 9);
  VERIFY(&*y - x == 9);
  VERIFY(y - &*x == 9);
  
  size_t s(y - x);
  VERIFY(s == 9);
}


struct A {
  mutable int i;
};
struct B : public A{
  mutable int j;
};
typedef _Pointer_adapter<_Relative_pointer_impl<B> >       B_pointer; 
typedef _Pointer_adapter<_Relative_pointer_impl<A> >       A_pointer; 
typedef _Pointer_adapter<_Relative_pointer_impl<const A> > const_A_pointer; 
typedef _Pointer_adapter<_Relative_pointer_impl<const B> > const_B_pointer; 


// Test implicit conversion from B* to A*
void inc(_Pointer_adapter<_Relative_pointer_impl<A> > a) {
  a->i++;
}
// Test implicit conversion from B* to const B*
void inc2(_Pointer_adapter<_Relative_pointer_impl<const B> > b) {
  b->i++;
  b->j++;
}
// Test implicit conversion from B* to const A*
void inc3(_Pointer_adapter<_Relative_pointer_impl<const A> > a) {
  a->i++;
}

void test02() {
  B b;
  b.i = 2;
  b.j = 2;

  B_pointer Bptr(&b);
  VERIFY(Bptr->i == 2);
  Bptr->i++;
  VERIFY(b.i == 3);
  
  const_B_pointer cBptr(&b);
  b.i++;
  VERIFY(cBptr->i == 4);
  
  A_pointer Aptr(&b);
  b.i++;
  VERIFY(Aptr->i == 5);
  Aptr->i++;
  VERIFY(b.i == 6);
  
  const_A_pointer cAptr(&b);
  b.i++;
  VERIFY(cAptr->i == 7);

  const_B_pointer cBptr2(Bptr);
  b.i++;
  VERIFY(cBptr2->i == 8);
  
  A_pointer Aptr2(Bptr);
  b.i++;
  VERIFY(Aptr2->i == 9);
  Aptr2->i++;
  VERIFY(b.i == 10);

  const_A_pointer cAptr2(Bptr);
  b.i++;
  VERIFY(cAptr2->i == 11);

  // Implicit casting during invocation
  inc(Bptr);
  VERIFY(Bptr->i == 12);
  inc2(Bptr);
  VERIFY(Bptr->i == 13);
  VERIFY(Bptr->j == 3);
  inc3(Bptr);
  VERIFY(Bptr->i == 14);
}

void test03() {
  B b;
  B* bPtr = &b;
  A* aPtr __attribute__((unused)) = __static_pointer_cast<A*>(bPtr);
  const A *caPtr __attribute__((unused)) = __static_pointer_cast<const A*>(bPtr);
  const B *cbPtr __attribute__((unused)) = __static_pointer_cast<const B*>(bPtr);

  B_pointer Bptr2 = &b;

  const A* caPtr2 __attribute__((unused)) = __static_pointer_cast<const A*>(Bptr2);
  A * aPtr2 __attribute__((unused)) = __static_pointer_cast<A*>(Bptr2);
  const B* cbPtr2 __attribute__((unused)) = __const_pointer_cast<const B*>(Bptr2);

  const_A_pointer caPtr3 __attribute__((unused)) = __static_pointer_cast<const A*>(Bptr2);
  A_pointer aPtr3 __attribute__((unused)) = __static_pointer_cast<A*>(Bptr2);
  const_B_pointer cbPtr3 __attribute__((unused)) = __const_pointer_cast<const B*>(Bptr2);
}

// Confirm the usability of the __static_pointer_cast<> template function
// to transform between _Pointer_adapter and standard versions.
void test04() {
  B b;
  B_pointer bPtr = &b;

  A_pointer aPtr = __static_pointer_cast<A_pointer>(bPtr);
  VERIFY(aPtr == bPtr);
  B_pointer bPtr2 = __static_pointer_cast<B_pointer>(aPtr);
  VERIFY(bPtr2 == aPtr);

  A* aPtr3 = __static_pointer_cast<A*>(bPtr);
  VERIFY(aPtr3 == bPtr);
  B* bPtr3 = __static_pointer_cast<B*>(aPtr);
  VERIFY(bPtr3 == aPtr);
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  return 0;
}
