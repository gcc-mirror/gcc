// { dg-do run { target *-*-freebsd* *-*-netbsd* *-*-linux* *-*-solaris* *-*-cygwin *-*-darwin* alpha*-*-osf* mips-sgi-irix6* } }
// { dg-options " -std=gnu++0x -pthread" { target *-*-freebsd* *-*-netbsd* *-*-linux* alpha*-*-osf* mips-sgi-irix6* } }
// { dg-options " -std=gnu++0x -pthreads" { target *-*-solaris* } }
// { dg-options " -std=gnu++0x " { target *-*-cygwin *-*-darwin* } }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }
// { dg-require-atomic-builtins "" }

// Copyright (C) 2009 Free Software Foundation, Inc.
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


#include <future>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

using namespace __gnu_test;

int f1() { return 0; }
int& f2() { static int i; return i; }
void f3() { }
ClassType f4() { return ClassType(); }

struct Derived : AbstractClass {
  void rotate(int) { }
  Derived& operator()(int i) { rotate(i); return *this; }
} f5;

void test01()
{
  bool test __attribute__((unused)) = true;
  using std::packaged_task;

  packaged_task<int ()> p1(f1);
  VERIFY( static_cast<bool>(p1) );
  packaged_task<int& ()> p2(f2);
  VERIFY( static_cast<bool>(p2) );
  packaged_task<void ()> p3(f3);
  VERIFY( static_cast<bool>(p3) );
  packaged_task<ClassType ()> p4(f4);
  VERIFY( static_cast<bool>(p4) );
  packaged_task<AbstractClass& (int)> p5(f5);
  VERIFY( static_cast<bool>(p5) );
}

int main()
{
  test01();
  return 0;
}
