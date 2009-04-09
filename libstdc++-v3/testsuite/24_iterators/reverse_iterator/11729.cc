// 2005-10-04  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005, 2009 Free Software Foundation, Inc.
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

// 24.4.1.2 Reverse iterators

#include <vector>
#include <testsuite_hooks.h>

// libstdc++/11729
void test01()
{
  bool test __attribute__((unused)) = true;

  typedef std::vector<int> Vec;
  typedef Vec::reverse_iterator reverse_iterator;
  typedef Vec::const_reverse_iterator const_reverse_iterator;
  
  Vec v(2);

  reverse_iterator rbeg = v.rbegin();               
  reverse_iterator rend = v.rend();
  const_reverse_iterator constrbeg(rbeg);
  const_reverse_iterator constrend(rend);

  VERIFY( rbeg == constrbeg );
  VERIFY( constrend == rend );

  VERIFY( rbeg != constrend );
  VERIFY( constrbeg != rend );

  VERIFY( rbeg < constrend );
  VERIFY( constrbeg < rend );

  VERIFY( rend > constrbeg );
  VERIFY( constrend > rbeg );

  VERIFY( rend >= constrend );
  VERIFY( constrbeg >= rbeg );

  VERIFY( rbeg <= constrbeg );
  VERIFY( constrend <= rend );

  VERIFY( rbeg - constrbeg == 0 );
  VERIFY( constrend - rend == 0 );

  VERIFY( rend - constrbeg > 0 );
  VERIFY( constrend - rbeg > 0 );

  VERIFY( (constrbeg = rend) == rend );
}

int main() 
{ 
  test01();
  return 0;
}
