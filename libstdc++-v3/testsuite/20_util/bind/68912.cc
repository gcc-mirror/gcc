// Copyright (C) 2015-2020 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include<functional>

struct Wrong {};
struct A {};
struct B {};
struct C{};
struct D{};

struct X {
  A operator()(int, double) & { return {}; }
  Wrong operator()(int, double) && {return {}; }

  B operator()(int, double) const & { return {}; }
  Wrong operator()(int, double) const && {return {}; }

  C operator()(int, double) volatile & { return {}; }
  Wrong operator()(int, double) volatile && {return {}; }

  D operator()(int, double) const volatile & { return {}; }
  Wrong operator()(int, double) const volatile && {return {}; }
};

void test01()
{
  auto bound = std::bind(X{}, 5, std::placeholders::_1);
  A res = bound(1.0);
  const auto bound_c = bound;
  B res_c = bound_c(1.0);
#if __cplusplus <= 201402L
  volatile auto bound_v = bound;
  C res_v = bound_v(1.0);
  volatile const auto bound_cv = bound;
  D res_cv = bound_cv(1.0);
#endif
}
