// { dg-do compile { xfail *-*-* } }
// { dg-options "-std=gnu++0x" }

// Copyright (C) 2010 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_common_types.h>

int main()
{
  __gnu_test::constexpr_default_constructible test;
  test.operator()<std::weak_ptr<int>>();  // { dg-excess-errors "" }
  //  test.operator()<std::__weak_ptr<int>>();
  //  test.operator()<std::__weak_count<__gnu_cxx::__default_lock_policy>>();
  // test.operator()<std::_Sp_counted_base<__gnu_cxx::__default_lock_policy>>();
  return 0;
}
