// 2005-01-17  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.
//
// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <set>
#include <testsuite_hooks.h>

// A few tests for insert with hint, in the occasion of libstdc++/19422
// and libstdc++/19433.
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  multiset<int> ms0, ms1;
  multiset<int>::iterator iter1;
  
  ms0.insert(1);
  ms1.insert(ms1.end(), 1);
  VERIFY( ms0 == ms1 );

  ms0.insert(3);
  ms1.insert(ms1.begin(), 3);
  VERIFY( ms0 == ms1 );

  ms0.insert(4);
  iter1 = ms1.insert(ms1.end(), 4);
  VERIFY( ms0 == ms1 );

  ms0.insert(6);
  ms1.insert(iter1, 6);
  VERIFY( ms0 == ms1 );

  ms0.insert(2);
  ms1.insert(ms1.begin(), 2);
  VERIFY( ms0 == ms1 );

  ms0.insert(7);
  ms1.insert(ms1.end(), 7);
  VERIFY( ms0 == ms1 );

  ms0.insert(5);
  ms1.insert(ms1.find(4), 5);
  VERIFY( ms0 == ms1 );

  ms0.insert(0);
  ms1.insert(ms1.end(), 0);
  VERIFY( ms0 == ms1 );

  ms0.insert(8);
  ms1.insert(ms1.find(3), 8);
  VERIFY( ms0 == ms1 );
  
  ms0.insert(9);
  ms1.insert(ms1.end(), 9);
  VERIFY( ms0 == ms1 );

  ms0.insert(10);
  ms1.insert(ms1.begin(), 10);
  VERIFY( ms0 == ms1 );
}

int main ()
{
  test01();
  return 0;
}
