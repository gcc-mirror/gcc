// { dg-do run { target c++11 } }

// Copyright (C) 2008-2020 Free Software Foundation, Inc.
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

#include <functional>
#include <type_traits>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

using namespace __gnu_test;

struct X
{
  typedef int result_type;

  X() : bar(17) {}

  int foo(float x)                   { return truncate_float(x); }
  int foo_c(float x)  const          { return truncate_float(x); }
  int foo_v(float x)  volatile       { return truncate_float(x); }
  int foo_cv(float x) const volatile { return truncate_float(x); }
  int foo_varargs(float x, ...)      { return truncate_float(x); }

  int operator()(float x)
  {
    return foo(x) + 1;
  }

  int operator()(float x) const
  {
    return foo_c(x) + 2;
  }

  int bar;

 private:
  X(const X&);
  X& operator=(const X&);
};

int seventeen() { return 17; }

struct get_seventeen
{
  typedef int result_type;
  int operator()() const { return 17; }
};

void test01()
{
  using std::ref;
  using std::cref;

  ::get_seventeen get_sev;
  ::X x;
  ::X* xp = &x;
  int (::X::* p_foo)(float) = &::X::foo;
  int (::X::* p_foo_c)(float) const = &::X::foo_c;
  int (::X::* p_foo_v)(float) volatile = &::X::foo_v;
  int (::X::* p_foo_cv)(float) const volatile = &::X::foo_cv;
  int (::X::* p_foo_varargs)(float, ...) = &::X::foo_varargs;
  int ::X::* p_bar = &::X::bar;

  const float pi = 3.14;

  // Functions
  VERIFY(ref(truncate_float)(pi) == 3);
  VERIFY(ref(seventeen)() == 17);

  // Function pointers
  VERIFY(cref(truncate_float)(pi) == 3);
  VERIFY(cref(seventeen)() == 17);

  // Member function pointers
  VERIFY(ref(p_foo)(x, pi) == 3);
  VERIFY(ref(p_foo)(xp, pi) == 3);
  VERIFY(ref(p_foo_c)(x, pi) == 3);
  VERIFY(ref(p_foo_c)(xp, pi) == 3);
  VERIFY(ref(p_foo_v)(x, pi) == 3);
  VERIFY(ref(p_foo_v)(xp, pi) == 3);
  VERIFY(ref(p_foo_cv)(x, pi) == 3);
  VERIFY(ref(p_foo_cv)(xp, pi) == 3);
  VERIFY(ref(p_foo_varargs)(x, pi) == 3);
  VERIFY(ref(p_foo_varargs)(xp, pi, 1, 1) == 3);
  VERIFY(ref(p_foo_varargs)(x, pi, 1, 1) == 3);
  VERIFY(ref(p_foo_varargs)(xp, pi) == 3);

  // Member data pointers
  VERIFY(ref(p_bar)(x) == 17);
  VERIFY(ref(p_bar)(xp) == 17);

  // Function objects
  VERIFY(ref(get_sev)() == 17);
  VERIFY(cref(get_sev)() == 17);
  VERIFY(ref(x)(pi) == 4);
  VERIFY(cref(x)(pi) == 5);
}

int main()
{
  test01();
  return 0;
}
