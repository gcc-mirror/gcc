// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

// 2005-01-15 Douglas Gregor <dgregor@cs.indiana.edu>
//
// Copyright (C) 2005-2025 Free Software Foundation, Inc.
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

// Put reference_wrappers to function pointers into function<> wrappers
void test07()
{
  using std::function;
  using std::ref;
  using std::cref;
  using std::reference_wrapper;

  int (*fptr)(float) = __gnu_test::truncate_float;

  function<int(float)> f1(ref(fptr));
  VERIFY( f1 );
  VERIFY( !!f1 );
  VERIFY( !(f1 == 0) );
  VERIFY( !(0 == f1) );
  VERIFY( f1 != 0 );
  VERIFY( 0 != f1 );

  // Invocation
  VERIFY( f1(3.1f) == 3 );

  // target_type and target() functions
  const function<int(float)>& f1c = f1;
  using ref_wrapper_type = reference_wrapper<int(*)(float)>;
#if __cpp_rtti
  VERIFY( typeid(ref_wrapper_type) == f1.target_type() );
#endif
  VERIFY( f1.target<ref_wrapper_type>() != nullptr );
  VERIFY( wraps(f1, fptr) );
  VERIFY( wraps(f1c, fptr) );

  function<int(float)> f2(cref(fptr));
  VERIFY( f2 );
  VERIFY( !!f2 );
  VERIFY( !(f2 == 0) );
  VERIFY( !(0 == f2) );
  VERIFY( f2 != 0 );
  VERIFY( 0 != f2 );

  // Invocation
  VERIFY( f2(3.1f) == 3 );

  // target_type and target() functions
  const function<int(float)>& f2c = f2;
  using cref_wrapper_type = reference_wrapper<int(* const)(float)>;
#if __cpp_rtti
  VERIFY( typeid(cref_wrapper_type) == f2.target_type() );
#endif
  VERIFY( wraps(f2, as_const(fptr)) );
#if __cpp_rtti
  VERIFY( f2c.target_type() == f2.target_type() );
#endif
  VERIFY( wraps(f2c, as_const(fptr)) );
}

int main()
{
  test07();
  return 0;
}
