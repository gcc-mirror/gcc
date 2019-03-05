// Copyright (C) 2015-2019 Free Software Foundation, Inc.
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

struct X : public std::enable_shared_from_this<X> { };

bool
share_ownership(const std::shared_ptr<X>& p1, const std::shared_ptr<X>& p2)
{
  return !p1.owner_before(p2) && !p2.owner_before(p1);
}

void
test01()
{
  std::shared_ptr<X> p1(new X);
  VERIFY( share_ownership( p1->shared_from_this(), p1 ) );
  {
    std::shared_ptr<X> p2(p1.get(), [](X*){});
    // The weak_ptr member of the enable_shared_from_this base should not
    // be reset by creating a second control block that owns the pointer.
    VERIFY( share_ownership( p2->shared_from_this(), p1 ) );
  }
  VERIFY( share_ownership( p1->shared_from_this(), p1 ) );
}

int
main()
{
  test01();
}
