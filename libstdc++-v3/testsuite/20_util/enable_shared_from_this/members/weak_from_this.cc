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

// { dg-options "-std=gnu++17" }

#include <memory>
#include <testsuite_hooks.h>

#if __cpp_lib_enable_shared_from_this < 201603
# error "__cpp_lib_enable_shared_from_this < 201603"
#endif

struct X : public std::enable_shared_from_this<X> { };

static_assert( noexcept(std::declval<X&>().weak_from_this()) );
static_assert( noexcept(std::declval<const X&>().weak_from_this()) );

void
test01()
{
  std::shared_ptr<X> sp(new X);
  auto wp1 = sp->weak_from_this();
  std::weak_ptr<X> wp2 = sp;

  std::owner_less<> less;
  VERIFY( !less(wp1, wp2) && !less(wp2, wp1) );
  VERIFY( !less(wp1, wp2) && !less(wp2, wp1) );
}

int
main()
{
  test01();
}
