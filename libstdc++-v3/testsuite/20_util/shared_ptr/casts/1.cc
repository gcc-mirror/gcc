// { dg-options "-std=gnu++0x" }

// Copyright (C) 2006, 2007 Free Software Foundation
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 20.6.6.2.10 shared_ptr casts [util.smartptr.shared.cast]

#include <memory>
#include <testsuite_tr1.h>

// { dg-do compile }

struct MyP { virtual ~MyP() { }; };
struct MyDP : MyP { };

int main()
{
  using __gnu_test::check_ret_type;
  using std::shared_ptr;
  using std::static_pointer_cast;
  using std::const_pointer_cast;
  using std::dynamic_pointer_cast;

  shared_ptr<double> spd;
  shared_ptr<const int> spci;
  shared_ptr<MyP> spa;

  check_ret_type<shared_ptr<void> >(static_pointer_cast<void>(spd));
  check_ret_type<shared_ptr<int> >(const_pointer_cast<int>(spci));
  check_ret_type<shared_ptr<MyDP> >(static_pointer_cast<MyDP>(spa));  
}
