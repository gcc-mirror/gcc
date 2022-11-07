// 2006-09-24  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006-2022 Free Software Foundation, Inc.
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

// 20.6.6.3 Template class weak_ptr [util.smartptr.weak]

#include <memory>
#include <testsuite_tr1.h>

// { dg-do compile { target c++11 } }
// { dg-require-effective-target hosted }

int main()
{
  using __gnu_test::check_ret_type;
  using std::weak_ptr;
  using std::shared_ptr;

  weak_ptr<int> wp;
  check_ret_type<shared_ptr<int> >(wp.lock());
}
