// 2004-10-20  Benjamin Kosnik  <bkoz@redhat.com>
//
// Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.
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

// 6.2.2 Class template array

#include <tr1/array>
#include <stdexcept>
#include <testsuite_hooks.h>

void
test01() 
{ 
  const size_t len = 5;
  typedef std::tr1::array<int, len> array_type;
  bool test __attribute__((unused)) = true;
  array_type a = { { 0, 1, 2, 3, 4 } };

  array_type::iterator b = a.begin();
  array_type::iterator e = a.end();

  VERIFY( e != (b + a.size() - 1));
}

int main()
{
  test01();
  return 0;
}

