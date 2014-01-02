// { dg-options "-std=gnu++11" }
// { dg-do compile }

// Copyright (C) 2012-2014 Free Software Foundation, Inc.
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

// PR libstdc++/55463 Passing rvalue objects to std::mem_fn

#include <functional>

struct X
{
  int& func();
  char& func_c() const;
  short& func_v() volatile;
  double& func_cv() const volatile;

  int data;
};

struct Y : X { };

using CX = const X;
using CY = const Y;

using X_ptr = X*;

struct smart_ptr
{
  X& operator*() const;
};

std::reference_wrapper<X> ref();
std::reference_wrapper<const X> cref();
std::reference_wrapper<Y> yref();

void test01()
{
  int& i1 __attribute__((unused)) = std::mem_fn( &X::func )( X() );
  int& i2 __attribute__((unused)) = std::mem_fn( &X::func )( Y() );
  int& i3 __attribute__((unused)) = std::mem_fn( &X::func )( ref() );
  int& i4 __attribute__((unused)) = std::mem_fn( &X::func )( yref() );
  int& i5 __attribute__((unused)) = std::mem_fn( &X::func )( X_ptr() );
  int& i6 __attribute__((unused)) = std::mem_fn( &X::func )( smart_ptr() );

  char& c1 __attribute__((unused)) = std::mem_fn( &X::func_c )( X() );
  char& c2 __attribute__((unused)) = std::mem_fn( &X::func_c )( CX() );
  char& c3 __attribute__((unused)) = std::mem_fn( &X::func_c )( Y() );
  char& c4 __attribute__((unused)) = std::mem_fn( &X::func_c )( ref() );
  char& c5 __attribute__((unused)) = std::mem_fn( &X::func_c )( cref() );
  char& c6 __attribute__((unused)) = std::mem_fn( &X::func_c )( yref() );
  char& c7 __attribute__((unused)) = std::mem_fn( &X::func_c )( X_ptr() );
  char& c8 __attribute__((unused)) = std::mem_fn( &X::func_c )( smart_ptr() );

  short& s1 __attribute__((unused)) = std::mem_fn( &X::func_v )( X() );
  short& s2 __attribute__((unused)) = std::mem_fn( &X::func_v )( Y() );
  short& s3 __attribute__((unused)) = std::mem_fn( &X::func_v )( ref() );
  short& s4 __attribute__((unused)) = std::mem_fn( &X::func_v )( yref() );
  short& s5 __attribute__((unused)) = std::mem_fn( &X::func_v )( X_ptr() );
  short& s6 __attribute__((unused)) = std::mem_fn( &X::func_v )( smart_ptr() );

  double& d1 __attribute__((unused)) = std::mem_fn( &X::func_cv )( X() );
  double& d2 __attribute__((unused)) = std::mem_fn( &X::func_cv )( CX() );
  double& d3 __attribute__((unused)) = std::mem_fn( &X::func_cv )( Y() );
  double& d4 __attribute__((unused)) = std::mem_fn( &X::func_cv )( ref() );
  double& d5 __attribute__((unused)) = std::mem_fn( &X::func_cv )( cref() );
  double& d6 __attribute__((unused)) = std::mem_fn( &X::func_cv )( yref() );
  double& d7 __attribute__((unused)) = std::mem_fn( &X::func_cv )( X_ptr() );
  double& d8 __attribute__((unused))
    = std::mem_fn( &X::func_cv )( smart_ptr() );

  // [expr.mptr.oper]
  // The result of a .* expression whose second operand is a pointer to a
  // data member is of the same value category (3.10) as its first operand.
  int&& rval __attribute__((unused)) = std::mem_fn( &X::data )( X() );
  const int&& crval __attribute__((unused)) = std::mem_fn( &X::data )( CX() );
  int&& yrval __attribute__((unused)) = std::mem_fn( &X::data )( Y() );
  const int&& ycrval __attribute__((unused)) = std::mem_fn( &X::data )( CY() );

  int& val __attribute__((unused)) = std::mem_fn( &X::data )( ref() );
  const int& cval __attribute__((unused)) = std::mem_fn( &X::data )( cref() );
  int& yval __attribute__((unused)) = std::mem_fn( &X::data )( yref() );

  int& pval __attribute__((unused)) = std::mem_fn( &X::data )( X_ptr() );
  int& sval __attribute__((unused)) = std::mem_fn( &X::data )( smart_ptr() );
}

void test02()
{
  std::reference_wrapper<X> r = ref();
  X& x1 __attribute__((unused))
    = std::mem_fn( &std::reference_wrapper<X>::get )( r );
  const std::reference_wrapper<X> cr = ref();
  const X& x3 __attribute__((unused))
    = std::mem_fn( &std::reference_wrapper<X>::get )( cr );
  X& x2 __attribute__((unused))
    = std::mem_fn( &std::reference_wrapper<X>::get )( ref() );
}
