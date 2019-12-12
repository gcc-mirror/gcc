// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

// Copyright (C) 2016-2019 Free Software Foundation, Inc.
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

// 20.11.2.2.9 shared_ptr casts [util.smartptr.shared.cast]

#include <memory>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

struct MyP { virtual ~MyP() { }; };
struct MyDP : MyP { };

void
test01()
{
  using __gnu_test::check_ret_type;
  using std::shared_ptr;
  using std::reinterpret_pointer_cast;

  shared_ptr<double> spd;
  shared_ptr<const int> spci;
  shared_ptr<MyP> spa;

  check_ret_type<shared_ptr<void>>(reinterpret_pointer_cast<void>(spd));
  check_ret_type<shared_ptr<const short>>(reinterpret_pointer_cast<const short>(spci));
  check_ret_type<shared_ptr<MyDP>>(reinterpret_pointer_cast<MyDP>(spa));
}

void
test02()
{
  using std::shared_ptr;
  using std::reinterpret_pointer_cast;

  int* ptr = new int(2);
  shared_ptr<int> pi(ptr);
  auto pl = reinterpret_pointer_cast<long>(pi);
  VERIFY(pi.use_count() == 2);
  VERIFY(pl.use_count() == 2);
  VERIFY(pi.get() == ptr);
  VERIFY(reinterpret_cast<int*>(pl.get()) == ptr);
}

int main()
{
  test01();
  test02();
}
