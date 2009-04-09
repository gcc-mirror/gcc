// Copyright (C) 2004, 2009 Free Software Foundation, Inc.
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

// { dg-do compile }
// { dg-options "-D_GLIBCXX_CONCEPT_CHECKS" }

#include <algorithm>

class class1
{ };

class class2
{ };

bool 
comp(class1&, class2&)
{ return true; }

class1 a;
class2 b;

// http://gcc.gnu.org/ml/libstdc++/2004-10/msg00448.html
void test01()
{
  std::find_first_of(&a, &a, &b, &b, comp);
}

int main()
{
  test01();
  return 0;
}
