// { dg-options "-std=gnu++11" }
// { dg-do compile }

// Copyright (C) 2012 Free Software Foundation, Inc.
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

using CX = const X;

struct smart_ptr
{
  X& operator*() const;
};

std::reference_wrapper<X> ref();
std::reference_wrapper<const X> cref();

void test01()
{
  int& i1 = std::mem_fn( &X::func )( X() );
  int& i2 = std::mem_fn( &X::func )( smart_ptr() );
  int& i3 = std::mem_fn( &X::func )( ref() );

  char& c1 = std::mem_fn( &X::func_c )( X() );
  char& c2 = std::mem_fn( &X::func_c )( CX() );
  char& c3 = std::mem_fn( &X::func_c )( smart_ptr() );
  char& c4 = std::mem_fn( &X::func_c )( ref() );
  char& c5 = std::mem_fn( &X::func_c )( cref() );

  short& s1 = std::mem_fn( &X::func_v )( X() );
  short& s2 = std::mem_fn( &X::func_v )( smart_ptr() );
  short& s3 = std::mem_fn( &X::func_v )( ref() );

  double& d1 = std::mem_fn( &X::func_cv )( X() );
  double& d2 = std::mem_fn( &X::func_cv )( CX() );
  double& d3 = std::mem_fn( &X::func_cv )( smart_ptr() );
  double& d4 = std::mem_fn( &X::func_cv )( ref() );
  double& d5 = std::mem_fn( &X::func_cv )( cref() );

  // [expr.mptr.oper]
  // The result of a .* expression whose second operand is a pointer to a
  // data member is of the same value category (3.10) as its first operand.
  int&& rval = std::mem_fn( &X::data )( X() );
  const int&& crval = std::mem_fn( &X::data )( CX() );

  int& sval = std::mem_fn( &X::data )( smart_ptr() );

  int& val = std::mem_fn( &X::data )( ref() );
  const int& cval = std::mem_fn( &X::data )( cref() );
}

