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

struct secret {};

struct noncopyable_function_object_type
{
  noncopyable_function_object_type(secret) {}

  int operator()() const { return 42; }
  int operator()()       { return 17; }

 private:
  noncopyable_function_object_type();
  noncopyable_function_object_type(const noncopyable_function_object_type&);
  void operator=(const noncopyable_function_object_type&);
};

// Put reference_wrappers into function<> wrappers
void test06()
{
  using std::function;
  using std::ref;
  using std::cref;

  secret password;
  noncopyable_function_object_type x(password);

  function<int()> f(ref(x));
  VERIFY( f );
  VERIFY( f() == 17 );
#if __cpp_rtti
  VERIFY( f.target_type() == typeid(std::ref(x)) ); // LWG 2781
#endif
  VERIFY( wraps(f, x) );

  function<int()> g = f;
  VERIFY( g );
  VERIFY( g() == 17 );
#if __cpp_rtti
  VERIFY( g.target_type() == f.target_type() );
#endif
  VERIFY( wraps(g, x) );

  function<int()> h = cref(x);
  VERIFY( h );
  VERIFY( h() == 42 );
#if __cpp_rtti
  VERIFY( h.target_type() == typeid(std::cref(x)) );
#endif
  VERIFY( wraps(h, as_const(x)) );

  const function<int()>& hc = h;
#if __cpp_rtti
  VERIFY( hc.target_type() == h.target_type() );
#endif
  VERIFY( wraps(hc, as_const(x)) );
}

int main()
{
  test06();
  return 0;
}
