// 2003-05-29  Paolo Carlini  <pcarlini@unitus.it>

// Copyright (C) 2003 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 24.4.1.2 Reverse iterators

#include <iterator>
#include <vector>
#include <testsuite_hooks.h>

// libstdc++/10783
void test03()
{
  bool test __attribute__((unused)) = true;

  typedef std::vector<int> V;
  VERIFY( sizeof(V::iterator) == sizeof(V::reverse_iterator) );
}

int main() 
{ 
  test03();
  return 0;
}
