// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

// 2005-01-15 Douglas Gregor <dgregor@cs.indiana.edu>
//
// Copyright (C) 2005-2024 Free Software Foundation, Inc.
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

template<typename T>
  const T&
  as_const(T& t)
  { return t; }

// Check that f's target is a reference_wrapper bound to obj.
template<typename Function, typename T>
  bool
  wraps(Function& f, T& obj)
  {
    using ref_wrapper_type = std::reference_wrapper<T>;
    auto* p = f.template target<ref_wrapper_type>();
    return std::addressof(p->get()) == std::addressof(obj);
  }

// Put reference_wrappers to member pointers
void test08()
{
  using std::function;
  using std::ref;
  using std::cref;
  using std::reference_wrapper;
  using __gnu_test::X;

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
#if __cpp_rtti
  VERIFY( typeid(ref(X_bar)) == frm.target_type() );
#endif
  VERIFY( wraps(frm, X_bar) );

  function<int(X&)> fr(ref(X_foo));
  VERIFY( fr );
  VERIFY( fr(x) == 1 );
#if __cpp_rtti
  VERIFY( typeid(ref(X_foo)) == fr.target_type() );
#endif
  VERIFY( wraps(fr, X_foo) );

  function<int(const X&)> frc(ref(X_foo_c));
  VERIFY( frc );
  VERIFY( frc(x) == 2 );
#if __cpp_rtti
  VERIFY( typeid(ref(X_foo_c)) == frc.target_type() );
#endif
  VERIFY( wraps(frc, X_foo_c) );

  function<int(volatile X&)> frv(ref(X_foo_v));
  VERIFY( frv );
  VERIFY( frv(x) == 3 );
#if __cpp_rtti
  VERIFY( typeid(ref(X_foo_v)) == frv.target_type() );
#endif
  VERIFY( wraps(frv, X_foo_v) );

  function<int(const volatile X&)> frcv(ref(X_foo_cv));
  VERIFY( frcv );
  VERIFY( frcv(x) == 4 );
#if __cpp_rtti
  VERIFY( typeid(ref(X_foo_cv)) == frcv.target_type() );
#endif
  VERIFY( wraps(frcv, X_foo_cv) );

  function<int(X*)> grm(ref(X_bar));
  VERIFY( grm );
  VERIFY( grm(&x) == 17 );
#if __cpp_rtti
  VERIFY( typeid(ref(X_bar)) == grm.target_type() );
#endif
  VERIFY( wraps(grm, X_bar) );

  function<int(X*)> gr(ref(X_foo));
  VERIFY( gr );
  VERIFY( gr(&x) == 1 );
#if __cpp_rtti
  VERIFY( typeid(ref(X_foo)) == gr.target_type() );
#endif
  VERIFY( wraps(gr, X_foo) );

  function<int(const X*)> grc(ref(X_foo_c));
  VERIFY( grc );
  VERIFY( grc(&x) == 2 );
#if __cpp_rtti
  VERIFY( typeid(ref(X_foo_c)) == grc.target_type() );
#endif
  VERIFY( wraps(grc, X_foo_c) );

  function<int(volatile X*)> grv(ref(X_foo_v));
  VERIFY( grv );
  VERIFY( grv(&x) == 3 );
#if __cpp_rtti
  VERIFY( typeid(ref(X_foo_v)) == grv.target_type() );
#endif
  VERIFY( wraps(grv, X_foo_v) );

  function<int(const volatile X*)> grcv(ref(X_foo_cv));
  VERIFY( grcv );
  VERIFY( grcv(&x) == 4 );
#if __cpp_rtti
  VERIFY( typeid(ref(X_foo_cv)) == grcv.target_type() );
#endif
  VERIFY( wraps(grcv, X_foo_cv) );

  function<int(X&)> hrm(cref(X_bar));
  VERIFY( hrm );
  VERIFY( hrm(x) == 17 );
#if __cpp_rtti
  VERIFY( typeid(cref(X_bar)) == hrm.target_type() );
#endif
  VERIFY( wraps(hrm, as_const(X_bar)) );

  function<int(X&)> hr(cref(X_foo));
  VERIFY( hr );
  VERIFY( hr(x) == 1 );
#if __cpp_rtti
  VERIFY( typeid(cref(X_foo)) == hr.target_type() );
#endif
  VERIFY( wraps(hr, as_const(X_foo)) );

  function<int(const X&)> hrc(cref(X_foo_c));
  VERIFY( hrc );
  VERIFY( hrc(x) == 2 );
#if __cpp_rtti
  VERIFY( typeid(cref(X_foo_c)) == hrc.target_type() );
#endif
  VERIFY( wraps(hrc, as_const(X_foo_c)) );

  function<int(volatile X&)> hrv(cref(X_foo_v));
  VERIFY( hrv );
  VERIFY( hrv(x) == 3 );
#if __cpp_rtti
  VERIFY( typeid(cref(X_foo_v)) == hrv.target_type() );
#endif
  VERIFY( wraps(hrv, as_const(X_foo_v)) );

  function<int(const volatile X&)> hrcv(cref(X_foo_cv));
  VERIFY( hrcv );
  VERIFY( hrcv(x) == 4 );
#if __cpp_rtti
  VERIFY( typeid(cref(X_foo_cv)) == hrcv.target_type() );
#endif
  VERIFY( wraps(hrcv, as_const(X_foo_cv)) );
}

int main()
{
  test08();
  return 0;
}
