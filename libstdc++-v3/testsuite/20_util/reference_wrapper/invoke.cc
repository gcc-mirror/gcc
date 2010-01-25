// { dg-options "-std=gnu++0x" }

// Copyright (C) 2008, 2009, 2010 Free Software Foundation, Inc.
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

bool test __attribute__((unused)) = true;

struct X
{
  typedef int result_type;

  X() : bar(17) {}

  int foo(float x)                   { return truncate_float(x); }
  int foo_c(float x)  const          { return truncate_float(x); }
  int foo_v(float x)  volatile       { return truncate_float(x); }
  int foo_cv(float x) const volatile { return truncate_float(x); }

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

  const float pi = 3.14;

  // Functions
  VERIFY(ref(truncate_float)(pi) == 3);
  VERIFY(ref(seventeen)() == 17);

  // Function pointers
  VERIFY(cref(&truncate_float)(pi) == 3);
  VERIFY(cref(&seventeen)() == 17);

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
