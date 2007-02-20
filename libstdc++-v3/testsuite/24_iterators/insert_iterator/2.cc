// 2001-06-21  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2004, 2005 Free Software Foundation, Inc.
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

// 24.4.2.5 Template class insert_iterator

#include <iterator>
#include <list>

void test02()
{
  typedef std::insert_iterator<std::list<int> > iterator_type;

  std::list<int> li;
  std::list<int>::iterator liit = li.begin();
  iterator_type it01(li, liit);
  iterator_type it02 = std::inserter(li, liit);
}

int main() 
{ 
  test02();
  return 0;
}
