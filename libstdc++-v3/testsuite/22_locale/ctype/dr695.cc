// { dg-do compile }
// 2007-11-08  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2007 Free Software Foundation
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

// 22.2.1 The ctype category

#include <locale>

// DR 695.
void
test01()
{
  using namespace std;
  
  locale loc;
  const ctype<char>& ct = use_facet<ctype<char> >(loc);
  ct.table();
  ct.classic_table();
}
