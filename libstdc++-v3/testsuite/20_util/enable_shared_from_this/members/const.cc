// Copyright (C) 2015-2018 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_hooks.h>

template<typename T, typename U>
  bool
  share_ownership(const std::shared_ptr<T>& p1, const std::shared_ptr<U>& p2)
  {
    return !p1.owner_before(p2) && !p2.owner_before(p1);
  }

void
test01()
{
  struct X : public std::enable_shared_from_this<X> { };
  using CX = const X;
  std::shared_ptr<CX> p(new CX);
  VERIFY( share_ownership(p->shared_from_this(), p) );
  p.reset(new X);
  VERIFY( share_ownership(p->shared_from_this(), p) );
  auto p2 = std::const_pointer_cast<X>(p)->shared_from_this();
  VERIFY( share_ownership(p2, p) );
}

void
test02()
{
  struct X;
  using CX = const X;
  struct X : public std::enable_shared_from_this<CX> { };
  std::shared_ptr<CX> p(new X);
  VERIFY( share_ownership(p->shared_from_this(), p) );
  p.reset(new CX);
  VERIFY( share_ownership(p->shared_from_this(), p) );
}

int
main()
{
  test01();
  test02();
}
