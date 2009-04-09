// { dg-options "-std=gnu++0x" }

// Copyright (C) 2008, 2009 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without Pred the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

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
  fwd_list_type::const_iterator posa = a.cbefore_begin();

  fwd_list_type x = {666.0, 777.0, 888.0};

  a.splice_after(posa, x);

  ++posa;
  VERIFY(*posa == 666.0);

  VERIFY(x.empty() == true);
}

// This test verifies the following:
//   
void
test02()
{
  typedef std::forward_list<double, _ExtPtr_allocator<double> > fwd_list_type;

  fwd_list_type a = {0.0, 1.0, 2.0, 3.0, 4.0};
  fwd_list_type::const_iterator posa = a.cbefore_begin();
  ++posa;
  VERIFY(*posa == 0.0);

  fwd_list_type y = {10.0, 11.0, 12.0, 13.0, 14.0, 15.0};
  fwd_list_type::const_iterator befy = y.cbefore_begin();
  ++befy;
  VERIFY(*befy == 10.0);
  fwd_list_type::const_iterator endy = befy;
  ++endy;
  ++endy;
  ++endy;
  ++endy;
  VERIFY(*endy == 14.0);

  a.splice_after(posa, y, befy, endy);
  VERIFY(*posa == 0.0);

  VERIFY(*befy == 10.0);
  ++befy;
  VERIFY(*befy == 15.0);
}

// This test verifies the following:
//   
void
test03()
{
  typedef std::forward_list<double, _ExtPtr_allocator<double> > fwd_list_type;

  fwd_list_type a = {0.0, 1.0, 2.0, 3.0, 4.0};
  fwd_list_type::const_iterator posa = a.cbefore_begin();
  ++posa;
  ++posa;
  VERIFY(*posa == 1.0);

  fwd_list_type z = {42.0, 43.0, 44.0};
  fwd_list_type::const_iterator posz = z.begin();
  VERIFY(*posz == 42.0);

  a.splice_after(posa, z, posz);
  VERIFY(*posa == 1.0);
  ++posa;
  VERIFY(*posa == 43.0);

  VERIFY(*posz == 42.0);
  ++posz;
  VERIFY(*posz == 44.0);
}

int
main()
{
  test01();
  test02();
  test03();
  return 0;
}
