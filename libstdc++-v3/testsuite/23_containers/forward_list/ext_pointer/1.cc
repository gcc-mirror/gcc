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

// 23.2.3.n forward_list capacity [lib.forward_list.capacity]

#include <forward_list>
#include <ext/extptr_allocator.h>
#include <testsuite_hooks.h>

bool test __attribute__((unused)) = true;

using __gnu_cxx::_ExtPtr_allocator;

// This test verifies the following.
//
void
test01()
{
  std::forward_list<double, _ExtPtr_allocator<double> > fld;

  VERIFY(fld.empty() == true);
  fld.push_front(1.0);
  VERIFY(fld.empty() == false);
  fld.resize(0);
  VERIFY(fld.empty() == true);
}

void
test02()
{
  std::forward_list<int, _ExtPtr_allocator<int> > a, b;
  a.push_front(1);

  b = std::move(a);
  VERIFY(b.empty() == false);
  VERIFY(*b.begin() == 1);
  VERIFY(a.empty() == true);

  std::forward_list<int, _ExtPtr_allocator<int> > c(std::move(b));
  VERIFY(c.empty() == false);
  (*c.begin() == 1 );
  VERIFY( b.empty() == true );
}

// Test various constrcutors 
void
test03()
{
  const int ni = 10;
  int i[ni] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};

  _ExtPtr_allocator<int>  alloc;

  std::forward_list<int, _ExtPtr_allocator<int> > flccin(i, i+ni);
  std::forward_list<int, _ExtPtr_allocator<int> > flc(flccin);
  std::forward_list<int, _ExtPtr_allocator<int> > flm(std::move(flccin));
  std::forward_list<int, _ExtPtr_allocator<int> > flcc(flccin, alloc );
  std::forward_list<int, _ExtPtr_allocator<int> > flmc(
	std::forward_list<int, _ExtPtr_allocator<int> >(i, i+ni), alloc);
  std::forward_list<double, _ExtPtr_allocator<double> > flil(
        {1.0, 2.0, 3.0, 4.0, 5.0});
}

// Test constrcutors 
//   Construction from given number of default item
//   Construction from given number of given item
void
test04()
{
  std::forward_list<double, _ExtPtr_allocator<double> > flvd(10);
  std::forward_list<float, _ExtPtr_allocator<float> > flv(10, 5.0F);
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  return 0;
}
