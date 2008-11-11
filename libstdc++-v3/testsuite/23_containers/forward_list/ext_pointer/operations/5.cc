// { dg-options "-std=gnu++0x" }

// Copyright (C) 2008 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without Pred the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 23.2.3.n forward_list xxx [lib.forward_list.xxx]

#include <forward_list>
#include <testsuite_hooks.h>
#include <ext/extptr_allocator.h>

using __gnu_cxx::_ExtPtr_allocator;

bool test __attribute__((unused)) = true;

// This test verifies the following:
//   
void
test01()
{
  typedef std::forward_list<double, _ExtPtr_allocator<double> > fwd_list_type;

  fwd_list_type a = {0.0, 1.0, 2.0, 3.0, 4.0};
  fwd_list_type b = {1.0, 2.0, 3.0, 4.0, 4.0, 5.0};

  a.merge(b);

  fwd_list_type r = {0.0, 1.0, 1.0, 2.0, 2.0, 3.0, 3.0, 4.0, 4.0, 4.0, 5.0};

  VERIFY(a == r);
}

int
main()
{
  test01();
  return 0;
}
