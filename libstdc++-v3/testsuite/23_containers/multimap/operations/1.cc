// 2006-11-25  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006, 2007 Free Software Foundation, Inc.
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

#include <map>
#include <testsuite_hooks.h>

// A few tests for equal_range, in the occasion of libstdc++/29385.
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  multimap<int, int> mm0;
  typedef multimap<int, int>::iterator iterator;
  pair<iterator, iterator> pp0;
  typedef multimap<int, int>::value_type value_type;

  pp0 = mm0.equal_range(1);
  VERIFY( mm0.count(1) == 0 );
  VERIFY( pp0.first == mm0.end() );
  VERIFY( pp0.second == mm0.end() );

  iterator iter0 = mm0.insert(value_type(1, 1));
  iterator iter1 = mm0.insert(value_type(2, 2));
  iterator iter2 = mm0.insert(value_type(3, 3));
 
  pp0 = mm0.equal_range(2);
  VERIFY( mm0.count(2) == 1 );
  VERIFY( *pp0.first == value_type(2, 2) );
  VERIFY( *pp0.second == value_type(3, 3) );
  VERIFY( pp0.first == iter1 );
  VERIFY( --pp0.first == iter0 );  
  VERIFY( pp0.second == iter2 );

  mm0.insert(value_type(3, 4));
  iterator iter3 = mm0.insert(value_type(3, 5));
  iterator iter4 = mm0.insert(value_type(4, 6));

  pp0 = mm0.equal_range(3);
  VERIFY( mm0.count(3) == 3 );
  VERIFY( *pp0.first == value_type(3, 3) );
  VERIFY( *pp0.second == value_type(4, 6) );
  VERIFY( pp0.first == iter2 );
  VERIFY( --pp0.first == iter1 );  
  VERIFY( pp0.second == iter4 );

  iterator iter5 = mm0.insert(value_type(0, 7));
  mm0.insert(value_type(1, 8));
  mm0.insert(value_type(1, 9));
  mm0.insert(value_type(1, 10));

  pp0 = mm0.equal_range(1);
  VERIFY( mm0.count(1) == 4 );
  VERIFY( *pp0.first == value_type(1, 1) );
  VERIFY( *pp0.second == value_type(2, 2) );
  VERIFY( pp0.first == iter0 );
  VERIFY( --pp0.first == iter5 );  
  VERIFY( pp0.second == iter1 );

  iterator iter6 = mm0.insert(value_type(5, 11));
  mm0.insert(value_type(5, 12));
  mm0.insert(value_type(5, 13));

  pp0 = mm0.equal_range(5);
  VERIFY( mm0.count(5) == 3 );
  VERIFY( *pp0.first == value_type(5, 11) );
  VERIFY( pp0.first == iter6 );
  VERIFY( --pp0.first == iter4 );  
  VERIFY( pp0.second == mm0.end() );

  mm0.insert(value_type(4, 14));
  mm0.insert(value_type(4, 15));
  mm0.insert(value_type(4, 16));

  pp0 = mm0.equal_range(4);
  VERIFY( mm0.count(4) == 4 );  
  VERIFY( *pp0.first == value_type(4, 6) );
  VERIFY( *pp0.second == value_type(5, 11) );  
  VERIFY( pp0.first == iter4 );
  VERIFY( --pp0.first == iter3 );  
  VERIFY( pp0.second == iter6 );

  mm0.insert(value_type(0, 17));
  iterator iter7 = mm0.insert(value_type(0, 18));
  mm0.insert(value_type(1, 19));

  pp0 = mm0.equal_range(0);
  VERIFY( mm0.count(0) == 3 );  
  VERIFY( *pp0.first == value_type(0, 7) );
  VERIFY( *pp0.second == value_type(1, 1) );  
  VERIFY( pp0.first == iter5 );
  VERIFY( pp0.first == mm0.begin() );
  VERIFY( pp0.second == iter0 );

  pp0 = mm0.equal_range(1);
  VERIFY( mm0.count(1) == 5 );  
  VERIFY( *pp0.first == value_type(1, 1) );
  VERIFY( *pp0.second == value_type(2, 2) );  
  VERIFY( pp0.first == iter0 );
  VERIFY( --pp0.first == iter7 );
  VERIFY( pp0.second == iter1 );
}

int
main()
{
  test01();
  return 0;
}
