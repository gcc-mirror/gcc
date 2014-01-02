// 2005-01-15 Douglas Gregor <dgregor@cs.indiana.edu>
//
// Copyright (C) 2005-2014 Free Software Foundation, Inc.
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

// 3.7.2 polymorphic function object wrapper
#include <tr1/functional>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

using namespace __gnu_test;

bool test __attribute__((unused)) = true;

// Put reference_wrappers to member pointers
void test08()
{
  using std::tr1::function;
  using std::tr1::ref;
  using std::tr1::cref;

  int X::* X_bar = &X::bar;
  int (X::* X_foo)() = &X::foo;
  int (X::* X_foo_c)() const = &X::foo_c;
  int (X::* X_foo_v)() volatile = &X::foo_v;
  int (X::* X_foo_cv)() const volatile = &X::foo_cv;

  X x;
  x.bar = 17;

  function<int(X&)> frm(ref(X_bar));
  VERIFY( frm );
  VERIFY( frm(x) == 17 );
  VERIFY( typeid(int X::*) == frm.target_type() );
  VERIFY( frm.target<int X::*>() == &X_bar );

  function<int(X&)> fr(ref(X_foo));
  VERIFY( fr );
  VERIFY( fr(x) == 1 );
  VERIFY( typeid(int (X::*)()) == fr.target_type() );
  VERIFY( fr.target<int (X::*)()>() == &X_foo );

  function<int(const X&)> frc(ref(X_foo_c));
  VERIFY( frc );
  VERIFY( frc(x) == 2 );
  VERIFY( typeid(int (X::*)() const) == frc.target_type() );
  VERIFY( frc.target<int (X::*)() const >() == &X_foo_c );

  function<int(volatile X&)> frv(ref(X_foo_v));
  VERIFY( frv );
  VERIFY( frv(x) == 3 );
  VERIFY( typeid(int (X::*)() volatile) == frv.target_type() );
  VERIFY( *frv.target<int (X::*)() volatile >() == X_foo_v );
  VERIFY( frv.target<int (X::*)() const volatile>() == 0 );

  function<int(const volatile X&)> frcv(ref(X_foo_cv));
  VERIFY( frcv );
  VERIFY( frcv(x) == 4 );
  VERIFY( typeid(int (X::*)() const volatile) == frcv.target_type() );
  VERIFY( *frcv.target<int (X::*)() const volatile >() == X_foo_cv );
  VERIFY( frcv.target<int (X::*)() const>() == 0 );

  function<int(X*)> grm(ref(X_bar));
  VERIFY( grm );
  VERIFY( grm(&x) == 17 );
  VERIFY( typeid(int X::*) == grm.target_type() );
  VERIFY( *grm.target<int X::*>() == X_bar );

  function<int(X*)> gr(ref(X_foo));
  VERIFY( gr );
  VERIFY( gr(&x) == 1 );
  VERIFY( typeid(int (X::*)()) == gr.target_type() );
  VERIFY( *gr.target<int (X::*)()>() == X_foo );

  function<int(const X*)> grc(ref(X_foo_c));
  VERIFY( grc );
  VERIFY( grc(&x) == 2 );
  VERIFY( typeid(int (X::*)() const) == grc.target_type() );
  VERIFY( *grc.target<int (X::*)() const >() == X_foo_c );

  function<int(volatile X*)> grv(ref(X_foo_v));
  VERIFY( grv );
  VERIFY( grv(&x) == 3 );
  VERIFY( typeid(int (X::*)() volatile) == grv.target_type() );
  VERIFY( *grv.target<int (X::*)() volatile >() == X_foo_v );
  VERIFY( grv.target<int (X::*)() const volatile>() == 0 );

  function<int(const volatile X*)> grcv(ref(X_foo_cv));
  VERIFY( grcv );
  VERIFY( grcv(&x) == 4 );
  VERIFY( typeid(int (X::*)() const volatile) == grcv.target_type() );
  VERIFY( *grcv.target<int (X::*)() const volatile >() == X_foo_cv );
  VERIFY( grcv.target<int (X::*)() const>() == 0 );

  function<int(X&)> hrm(cref(X_bar));
  VERIFY( hrm );
  VERIFY( hrm(x) == 17 );
  VERIFY( typeid(int X::*) == hrm.target_type() );
  VERIFY( hrm.target<int X::*>() == 0 );
  VERIFY( hrm.target<int X::* const>() == &X_bar );

  function<int(X&)> hr(cref(X_foo));
  VERIFY( hr );
  VERIFY( hr(x) == 1 );
  VERIFY( typeid(int (X::*)()) == hr.target_type() );
  VERIFY( hr.target<int (X::* const)()>() == &X_foo );

  function<int(const X&)> hrc(cref(X_foo_c));
  VERIFY( hrc );
  VERIFY( hrc(x) == 2 );
  VERIFY( typeid(int (X::*)() const) == hrc.target_type() );
  VERIFY( hrc.target<int (X::* const)() const >() == &X_foo_c );

  function<int(volatile X&)> hrv(cref(X_foo_v));
  VERIFY( hrv );
  VERIFY( hrv(x) == 3 );
  VERIFY( typeid(int (X::*)() volatile) == hrv.target_type() );
  VERIFY( hrv.target<int (X::* const)() volatile >() == &X_foo_v );
  VERIFY( hrv.target<int (X::* const)() const volatile>() == 0 );

  function<int(const volatile X&)> hrcv(cref(X_foo_cv));
  VERIFY( hrcv );
  VERIFY( hrcv(x) == 4 );
  VERIFY( typeid(int (X::*)() const volatile) == hrcv.target_type() );
  VERIFY( hrcv.target<int (X::* const)() const volatile >() == &X_foo_cv );
  VERIFY( hrcv.target<int (X::* const)() const>() == 0 );
}

int main()
{
  test08();
  return 0;
}
