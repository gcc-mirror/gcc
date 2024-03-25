// { dg-do compile { target c++11 } }
// { dg-require-effective-target hosted }
//
// Copyright (C) 2012-2024 Free Software Foundation, Inc.
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

// 20.8.11 polymorphic function object wrapper

#include <functional>

struct X { void f() { } };

void (X::*p)() = &X::f;
void (X::* volatile& vp)() = p;

typedef std::function<void(X&)> function_type;

void test01()
{
  function_type f( vp );
  function_type f2( std::ref(vp) );
  function_type f3( std::cref(vp) );
}
