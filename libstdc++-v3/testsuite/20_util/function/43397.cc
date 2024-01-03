// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

// Copyright (C) 2010-2024 Free Software Foundation, Inc.
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

// 20.8.15 polymorphic function object wrapper

#include <functional>
#include <testsuite_hooks.h>

struct Foo
{
  Foo() { }
  short operator() ( int && ) { return 1; }
  short operator() ( int && ) const { return 2; }
  short operator() ( int && ) volatile { return 3; }
  short operator() ( int && ) const volatile { return 4; }
  short func( int && ) { return 5; }
  short func_c( int && ) const { return 6; }
  short func_v( int && ) volatile { return 7; }
  short func_cv( int && ) const volatile { return 8; }
};

void test01()
{
  using std::function;
  using std::ref;

  Foo foo;
  Foo const foo_c;
  Foo volatile foo_v;
  Foo const volatile foo_cv;

  std::function< int ( int && ) > f1( ref(foo) );
  VERIFY( f1(0) == 1 );

  std::function< int ( int && ) > f2( ref(foo_c) );
  VERIFY( f2(0) == 2 );

  std::function< int ( int && ) > f3( ref(foo_v) );
  VERIFY( f3(0) == 3 );

  std::function< int ( int && ) > f4( ref(foo_cv) );
  VERIFY( f4(0) == 4 );

  std::function< int ( Foo &, int && ) > f5( &Foo::func ) ;
  VERIFY( f5(foo, 0) == 5 );

  std::function< int ( Foo const &, int && ) > f6( &Foo::func_c ) ;
  VERIFY( f6(foo_c, 0) == 6 );

  std::function< int ( Foo volatile &, int && ) > f7( &Foo::func_v ) ;
  VERIFY( f7(foo_v, 0) == 7 );

  std::function< int ( Foo const volatile &, int && ) > f8( &Foo::func_cv ) ;
  VERIFY( f8(foo_cv, 0) == 8 );
}

int main()
{
  test01();
  return 0;
}
