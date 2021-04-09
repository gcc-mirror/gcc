// { dg-do run { target c++11 } }

// Copyright (C) 2005-2021 Free Software Foundation, Inc.
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


#include <deque>
#include <testsuite_hooks.h>
#include <testsuite_rvalref.h>

using namespace __gnu_test;

// Test deque::push_back makes no unneeded copies.
void
test01()
{
  std::deque<copycounter> a;
  copycounter c(1);
  copycounter::copycount = 0;
  for(int i = 0; i < 1000; ++i)
    a.push_back(c);
  VERIFY(copycounter::copycount == 1000);
}

// Test deque::push_front makes no unneeded copies.
void
test02()
{
  std::deque<copycounter> a;
  copycounter c(1);
  copycounter::copycount = 0;
  for(int i = 0; i < 1000; ++i)
    a.push_front(c);
  VERIFY(copycounter::copycount == 1000);
}

// Test deque::insert makes no unneeded copies.
void
test03()
{
  std::deque<copycounter> a(1000);
  copycounter c(1);
  copycounter::copycount = 0;
  a.insert(a.begin(),c);
  a.insert(a.end(),c);
  for(int i = 0; i < 500; ++i)
    a.insert(a.begin() + i, c);
  VERIFY(copycounter::copycount == 502);
}

// Test deque::insert(iterator, count, value) makes no unneeded copies
// when it has to also reallocate the deque's internal buffer.
void
test04()
{
  copycounter c(1);
  std::deque<copycounter> a(10, c);
  copycounter::copycount = 0;
  a.insert(a.begin(), 20, c);
  VERIFY(copycounter::copycount == 20);
  a.insert(a.end(), 50, c);
  VERIFY(copycounter::copycount == 70);
  // NOTE : These values are each one higher than might be expected, as
  // deque::insert(iterator, count, value) copies the value it is given
  // when it has to move elements in the deque in case that value is
  // in the deque.
  
  // Near the start
  a.insert(a.begin() + 10, 100, c);
  VERIFY(copycounter::copycount == 170 + 1);
  // Near the end
  a.insert(a.end() - 10, 1000, c);
  VERIFY(copycounter::copycount == 1170 + 2);
}

// Test deque::insert(iterator, count, value) makes no unneeded copies
// when it doesn't have to reallocate the deque's internal buffer.
void
test05()
{
  copycounter c(1);
  std::deque<copycounter> a(10, c);
  copycounter::copycount = 0;
  //a.reserve(1000);
  a.insert(a.begin(), 20, c);
  VERIFY(copycounter::copycount == 20 );
  a.insert(a.end(), 50, c);
  VERIFY(copycounter::copycount == 70 );
  
  // NOTE : These values are each one higher than might be expected, as
  // deque::insert(iterator, count, value) copies the value it is given
  // when it has to move elements in the deque in case that value is
  // in the deque.
  // Near the start
  a.insert(a.begin() + 10, 100, c);
  VERIFY(copycounter::copycount == 170 + 1);
  // Near the end
  a.insert(a.end() - 10, 200, c);
  VERIFY(copycounter::copycount == 370 + 2);
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  return 0;
}
