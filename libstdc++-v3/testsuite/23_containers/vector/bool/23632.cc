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

// { dg-do compile }

#include <vector>

// libstdc++/23632
void test01()
{
  std::vector<bool> v(100);
  const std::vector<bool>::iterator fu = v.begin();
  if (!fu[0])
    fu[0] = true;

  const std::vector<bool>::const_iterator cfu = v.begin();
  if (cfu[0])
    ;
}

int main()
{
  test01();
  return 0;
}
