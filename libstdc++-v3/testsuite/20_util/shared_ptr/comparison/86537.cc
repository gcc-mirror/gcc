// Copyright (C) 2018-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

#include <memory>
#include <testsuite_hooks.h>

struct Should_not_happen { };

struct X { };

namespace std {
  template<> struct less<X*> {
    bool operator()(X*, X*) const { throw Should_not_happen(); }
  };
}

bool custom_op_called = false;

bool
operator<(const std::shared_ptr<X>&, const std::shared_ptr<X>&)
{
  custom_op_called = true;
  return false;
}

void
test01()
{
  const std::shared_ptr<X> sp;
  bool b = sp < sp;
  VERIFY( !b );
  VERIFY( custom_op_called );

  std::less<std::shared_ptr<X>> lt;
  custom_op_called = false;
  b = lt(sp, sp);
  VERIFY( !b );
  VERIFY( custom_op_called ); // PR libstdc++/86537 and LWG DR 1262

#if __cplusplus >= 201402L
  std::less<> ltv;
  custom_op_called = false;
  b = ltv(sp, sp);
  VERIFY( !b );
  VERIFY( custom_op_called );
#endif
}

int
main()
{
  test01();
}
