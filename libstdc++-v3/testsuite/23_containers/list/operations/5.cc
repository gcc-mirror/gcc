// 2006-01-19  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006, 2009 Free Software Foundation, Inc.
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

// 23.2.2.4 list operations [lib.list.ops]

#include <list>
#include <stdexcept>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

// Check the splice (and merge) bits of N1599.
void
test01()
{
  bool test __attribute__((unused)) = true;
  
  typedef __gnu_test::uneq_allocator<int> my_alloc;
  typedef std::list<int, my_alloc> my_list;

  const int data1[] = {1, 2, 3, 4, 5};
  const int data2[] = {6, 7, 8, 9, 10};
  const size_t N1 = sizeof(data1) / sizeof(int);
  const size_t N2 = sizeof(data2) / sizeof(int);
  
  my_alloc alloc01(1), alloc02(2);

  my_list l01(data1, data1 + N1, alloc01);
  const my_list l01_ref = l01;

  my_list l02(data2, data2 + N2, alloc02);
  const my_list l02_ref = l02;

  bool catched = false;

  try
    {
      l01.splice(l01.begin(), l02);
    }
  catch(std::runtime_error&)
    {
      catched = true;
    }
  catch(...)
    {
      VERIFY( false );
    }
  VERIFY( catched );
  VERIFY( l01 == l01_ref );
  VERIFY( l02 == l02_ref );
  
  catched = false;
  try
    {
      l01.splice(l01.begin(), l02, l02.begin());
    }
  catch(std::runtime_error&)
    {
      catched = true;
    }
  catch(...)
    {
      VERIFY( false );
    }
  VERIFY( catched );
  VERIFY( l01 == l01_ref );
  VERIFY( l02 == l02_ref );

  catched = false;
  try
    {
      l01.splice(l01.begin(), l02, l02.begin(), l02.end());
    }
  catch(std::runtime_error&)
    {
      catched = true;
    }
  catch(...)
    {
      VERIFY( false );
    }
  VERIFY( catched );
  VERIFY( l01 == l01_ref );
  VERIFY( l02 == l02_ref );

  catched = false;
  try
    {
      l01.merge(l02);
    }
  catch(std::runtime_error&)
    {
      catched = true;
    }
  catch(...)
    {
      VERIFY( false );
    }
  VERIFY( catched );
  VERIFY( l01 == l01_ref );
  VERIFY( l02 == l02_ref );

  catched = false;
  try
    {
      l01.merge(l02, std::less<int>());
    }
  catch(std::runtime_error&)
    {
      catched = true;
    }
  catch(...)
    {
      VERIFY( false );
    }
  VERIFY( catched );
  VERIFY( l01 == l01_ref );
  VERIFY( l02 == l02_ref );
}

int main()
{
  test01();
  return 0;
}
