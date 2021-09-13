// Copyright (C) 2015-2021 Free Software Foundation, Inc.
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

// 8.2.1.3 shared_ptr casts [memory.smartptr.shared.cast]

#include <experimental/memory>
#include <testsuite_tr1.h>

// { dg-do compile { target c++14 } }

struct A { };

int
main()
{
  using __gnu_test::check_ret_type;
  using std::experimental::shared_ptr;
  using std::experimental::static_pointer_cast;
  using std::experimental::const_pointer_cast;
  using std::experimental::dynamic_pointer_cast;

  shared_ptr<A[5]> spa;
  shared_ptr<const A[5]> spa1;

  check_ret_type<shared_ptr<A[]> >(static_pointer_cast<A[]>(spa));
  check_ret_type<shared_ptr<A[]> >(const_pointer_cast<A[]>(spa1));
  return 0;
}
