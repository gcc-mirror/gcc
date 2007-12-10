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

  map<int, int> m0;
  typedef map<int, int>::iterator iterator;
  typedef pair<iterator, bool> insert_return_type;
  pair<iterator, iterator> pp0;
  typedef map<int, int>::value_type value_type;

  pp0 = m0.equal_range(1);
  VERIFY( m0.count(1) == 0 );
  VERIFY( pp0.first == m0.end() );
  VERIFY( pp0.second == m0.end() );

  insert_return_type irt0 = m0.insert(value_type(1, 1));
  insert_return_type irt1 = m0.insert(value_type(2, 2));
  insert_return_type irt2 = m0.insert(value_type(3, 3));
 
  pp0 = m0.equal_range(2);
  VERIFY( m0.count(2) == 1 );
  VERIFY( *pp0.first == value_type(2, 2) );
  VERIFY( *pp0.second == value_type(3, 3) );
  VERIFY( pp0.first == irt1.first );
  VERIFY( --pp0.first == irt0.first );  
  VERIFY( pp0.second == irt2.first );

  m0.insert(value_type(3, 4));
  insert_return_type irt3 = m0.insert(value_type(3, 5));
  insert_return_type irt4 = m0.insert(value_type(4, 6));

  pp0 = m0.equal_range(3);
  VERIFY( m0.count(3) == 1 );
  VERIFY( *pp0.first == value_type(3, 3) );
  VERIFY( *pp0.second == value_type(4, 6) );
  VERIFY( pp0.first == irt2.first );
  VERIFY( --pp0.first == irt1.first );  
  VERIFY( pp0.second == irt4.first );

  insert_return_type irt5 = m0.insert(value_type(0, 7));
  m0.insert(value_type(1, 8));
  m0.insert(value_type(1, 9));
  m0.insert(value_type(1, 10));

  pp0 = m0.equal_range(1);
  VERIFY( m0.count(1) == 1 );
  VERIFY( *pp0.first == value_type(1, 1) );
  VERIFY( *pp0.second == value_type(2, 2) );
  VERIFY( pp0.first == irt0.first );
  VERIFY( --pp0.first == irt5.first );  
  VERIFY( pp0.second == irt1.first );

  insert_return_type irt6 = m0.insert(value_type(5, 11));
  m0.insert(value_type(5, 12));
  m0.insert(value_type(5, 13));

  pp0 = m0.equal_range(5);
  VERIFY( m0.count(5) == 1 );
  VERIFY( *pp0.first == value_type(5, 11) );
  VERIFY( pp0.first == irt6.first );
  VERIFY( --pp0.first == irt4.first );  
  VERIFY( pp0.second == m0.end() );

  m0.insert(value_type(4, 14));
  m0.insert(value_type(4, 15));
  m0.insert(value_type(4, 16));

  pp0 = m0.equal_range(4);
  VERIFY( m0.count(4) == 1 );  
  VERIFY( *pp0.first == value_type(4, 6) );
  VERIFY( *pp0.second == value_type(5, 11) );  
  VERIFY( pp0.first == irt4.first );
  VERIFY( --pp0.first == irt3.first );  
  VERIFY( pp0.second == irt6.first );

  m0.insert(value_type(0, 17));
  insert_return_type irt7 = m0.insert(value_type(0, 18));
  m0.insert(value_type(1, 19));

  pp0 = m0.equal_range(0);
  VERIFY( m0.count(0) == 1 );  
  VERIFY( *pp0.first == value_type(0, 7) );
  VERIFY( *pp0.second == value_type(1, 1) );  
  VERIFY( pp0.first == irt5.first );
  VERIFY( pp0.first == m0.begin() );
  VERIFY( pp0.second == irt0.first );

  pp0 = m0.equal_range(1);
  VERIFY( m0.count(1) == 1 );  
  VERIFY( *pp0.first == value_type(1, 1) );
  VERIFY( *pp0.second == value_type(2, 2) );  
  VERIFY( pp0.first == irt0.first );
  VERIFY( --pp0.first == irt7.first);
  VERIFY( pp0.second == irt1.first );
}

int
main()
{
  test01();
  return 0;
}
