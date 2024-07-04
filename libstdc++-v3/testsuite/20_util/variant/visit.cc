// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }

#include <variant>
#include <functional>
#include <testsuite_hooks.h>

// N.B. there are more std::visit tests in ./compile.cc and ./run.cc

void
test01()
{
  // Verify that visitation uses INVOKE and supports arbitrary callables.

  struct X
  {
    int sum(int i) const { return i + n; }
    int product(int i) const { return i * n; }
    int n;
  };

  std::variant<X, X*, std::reference_wrapper<X>> vobj{X{1}};
  int res = std::visit(&X::n, vobj);
  VERIFY( res == 1 );

  std::variant<int, short> varg{2};
  res = std::visit(&X::sum, vobj, varg);
  VERIFY( res == 3 );

  X x{4};
  vobj = &x;
  res = std::visit(&X::n, vobj);
  VERIFY( res == 4 );

  varg.emplace<short>(5);
  res = std::visit(&X::sum, vobj, varg);
  VERIFY( res == 9 );

  x.n = 6;
  res = std::visit(&X::product, vobj, varg);
  VERIFY( res == 30 );

  vobj = std::ref(x);
  x.n = 7;
  res = std::visit(&X::n, vobj);
  VERIFY( res == 7 );

  res = std::visit(&X::product, vobj, varg);
  VERIFY( res == 35 );
}

void
test02()
{
  struct NoCopy
  {
    NoCopy() { }
    NoCopy(const NoCopy&) = delete;
    NoCopy(NoCopy&&) = delete;
    ~NoCopy() { }

    int operator()(int i) { return i; }
    int operator()(const NoCopy&) { return 0; }
  };

  std::variant<NoCopy, int> v{1};
  NoCopy f;
  // Visit should not need arguments to be copyable:
  int res = std::visit(f, v);
  VERIFY( res == 1 );
}

int
main()
{
  test01();
  test02();
}
