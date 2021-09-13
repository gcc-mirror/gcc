// Copyright (C) 2021 Free Software Foundation, Inc.
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

// { dg-do run }

#include <deque>
#include <testsuite_hooks.h>

// PR libstdc++/70303

void test01()
{
  typedef typename std::deque<int>::iterator It;
  It it = It();
  VERIFY(it == it);
  VERIFY(!(it != it));
  VERIFY(it - it == 0);
  VERIFY(!(it < it));
  VERIFY(!(it > it));
  VERIFY(it <= it);
  VERIFY(it >= it);

  typedef typename std::deque<int>::const_iterator CIt;
  CIt cit = CIt();
  VERIFY(cit == cit);
  VERIFY(!(cit != cit));
  VERIFY(cit - cit == 0);
  VERIFY(!(cit < cit));
  VERIFY(!(cit > cit));
  VERIFY(cit <= cit);
  VERIFY(cit >= cit);

  VERIFY(it == cit);
  VERIFY(!(it != cit));
  VERIFY(cit == it);
  VERIFY(!(cit != it));
  VERIFY(it - cit == 0);
  VERIFY(cit - it == 0);
  VERIFY(!(it < cit));
  VERIFY(!(it > cit));
  VERIFY(it <= cit);
  VERIFY(it >= cit);
  VERIFY(!(cit < it));
  VERIFY(!(cit > it));
  VERIFY(cit <= it);
  VERIFY(cit >= it);
}

int main()
{
  test01();
  return 0;
}
