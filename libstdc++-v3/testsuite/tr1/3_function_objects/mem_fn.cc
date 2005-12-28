// 2005-01-26 Douglas Gregor <dgregor@cs.indiana.edu>
//
// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 3.5 function template mem_fn
#include <tr1/functional>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

struct X { int bar; };

struct Y : X { };

template<typename T>
struct dumb_ptr
{
  dumb_ptr(T* p) : p(p) {}

  T& operator*() const { return *p; }

 private:
  T* p;
};

// Test mem_fn with a data member
void test01(int r = 0)
{
  using std::tr1::mem_fn;

  X x;
  Y y;
  const X& xc = x;
  const Y& yc = y;
  X* xp = &x;
  Y* yp =&y;
  const X* xpc = xp;
  const Y* ypc = yp;
  dumb_ptr<X> xd(xp);
  dumb_ptr<Y> yd(yp);
  const dumb_ptr<X>& xdc = xd;
  const dumb_ptr<Y>& ydc = yd;

  int& bx = mem_fn(&X::bar)(x);
  const int& bxc = mem_fn(&X::bar)(xc);
  int& bxp = mem_fn(&X::bar)(xp);
  const int& bxpc = mem_fn(&X::bar)(xpc);
  const int& bxd = mem_fn(&X::bar)(xd);
  const int& bxdc = mem_fn(&X::bar)(xdc);

  int& by = mem_fn(&X::bar)(y);
  const int& byc = mem_fn(&X::bar)(yc);
  int& byp = mem_fn(&X::bar)(yp);
  const int& bypc = mem_fn(&X::bar)(ypc);
  const int& byd = mem_fn(&X::bar)(yd);
  const int& bydc = mem_fn(&X::bar)(ydc);
  
  // Avoid unused variable warnings.
  r = bx + bxc + bxp + bxpc + bxd + bxdc + by + byc + byp + bypc + byd + bydc;
}

int main()
{
  test01();
  return 0;
}
