// { dg-do run { target c++11 } }
// 2005-01-15 Douglas Gregor <dgregor@cs.indiana.edu>
//
// Copyright (C) 2005-2021 Free Software Foundation, Inc.
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

// 20.7.15 polymorphic function object wrapper
#include <functional>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

using namespace __gnu_test;

// Put member pointers into function<> wrappers
void test05()
{
  using std::function;

  X x;
  x.bar = 17;

  function<int(X&)> frm(&X::bar);
  VERIFY( frm );
  VERIFY( frm(x) == 17 );
  VERIFY( typeid(int X::*) == frm.target_type() );
  VERIFY( *frm.target<int X::*>() == &X::bar );

  function<int(X&)> fr(&X::foo);
  VERIFY( fr );
  VERIFY( fr(x) == 1 );
  VERIFY( typeid(int (X::*)()) == fr.target_type() );
  VERIFY( *fr.target<int (X::*)()>() == &X::foo );

  function<int(const X&)> frc(&X::foo_c);
  VERIFY( frc );
  VERIFY( frc(x) == 2 );
  VERIFY( typeid(int (X::*)() const) == frc.target_type() );
  VERIFY( *frc.target<int (X::*)() const >() == &X::foo_c );

  function<int(volatile X&)> frv(&X::foo_v);
  VERIFY( frv );
  VERIFY( frv(x) == 3 );
  VERIFY( typeid(int (X::*)() volatile) == frv.target_type() );
  VERIFY( *frv.target<int (X::*)() volatile >() == &X::foo_v );
  VERIFY( frv.target<int (X::*)() const volatile>() == 0 );

  function<int(const volatile X&)> frcv(&X::foo_cv);
  VERIFY( frcv );
  VERIFY( frcv(x) == 4 );
  VERIFY( typeid(int (X::*)() const volatile) == frcv.target_type() );
  VERIFY( *frcv.target<int (X::*)() const volatile >() == &X::foo_cv );
  VERIFY( frcv.target<int (X::*)() const>() == 0 );

  function<int(X*)> grm(&X::bar);
  VERIFY( grm );
  VERIFY( grm(&x) == 17 );
  VERIFY( typeid(int X::*) == grm.target_type() );
  VERIFY( *grm.target<int X::*>() == &X::bar );

  function<int(X*)> gr(&X::foo);
  VERIFY( gr );
  VERIFY( gr(&x) == 1 );
  VERIFY( typeid(int (X::*)()) == gr.target_type() );
  VERIFY( *gr.target<int (X::*)()>() == &X::foo );

  function<int(const X*)> grc(&X::foo_c);
  VERIFY( grc );
  VERIFY( grc(&x) == 2 );
  VERIFY( typeid(int (X::*)() const) == grc.target_type() );
  VERIFY( *grc.target<int (X::*)() const >() == &X::foo_c );

  function<int(volatile X*)> grv(&X::foo_v);
  VERIFY( grv );
  VERIFY( grv(&x) == 3 );
  VERIFY( typeid(int (X::*)() volatile) == grv.target_type() );
  VERIFY( *grv.target<int (X::*)() volatile >() == &X::foo_v );
  VERIFY( grv.target<int (X::*)() const volatile>() == 0 );

  function<int(const volatile X*)> grcv(&X::foo_cv);
  VERIFY( grcv );
  VERIFY( grcv(&x) == 4 );
  VERIFY( typeid(int (X::*)() const volatile) == grcv.target_type() );
  VERIFY( *grcv.target<int (X::*)() const volatile >() == &X::foo_cv );
  VERIFY( grcv.target<int (X::*)() const>() == 0 );
}

int main()
{
  test05();
  return 0;
}
