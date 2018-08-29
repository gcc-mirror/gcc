// { dg-do compile }

// 2007-03-03  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2007-2018 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <sstream>

class MyClass
{
  double x;

public:
  MyClass(double X) : x(X) {}
  friend bool operator&&(int i, const MyClass& Z);
};

inline bool
operator&&(int i, const MyClass& Z)
{ return int(Z.x) == i; }

// libstdc++/31031
void test01()
{
  int k =3;
  MyClass X(3.1);
  std::ostringstream oss;

  oss << (k && X);
}

int main()
{
  test01();
  return 0;
}
