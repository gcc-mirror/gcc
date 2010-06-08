// { dg-do compile }

// 2006-03-13  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006, 2009, 2010 Free Software Foundation, Inc.
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


// 27.4.3 template class fpos

#include <ios>

void test01()
{
  bool test01, test02;

  std::streampos pos01(-1), pos02(0);

  test01 = pos01 == -1;
  test01 = -1 == pos01;  
  test01 = test01; // Suppress unused warning.

  test02 = pos02 != -1;
  test02 = -1 != pos02;
  test02 = test02; // Suppress unused warning.
}
