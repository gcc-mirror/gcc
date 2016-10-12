// { dg-do run { target c++11 } }

// 2010-06-11  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2016 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_hooks.h>

void test01()
{
  struct T { };

  std::unique_ptr<T>                                 u0(new T);
  std::hash<std::unique_ptr<T>>                      hu0;
  std::hash<typename std::unique_ptr<T>::pointer>    hp0;

  VERIFY( hu0(u0) == hp0(u0.get()) );

  std::unique_ptr<T[]>                               u1(new T[10]);
  std::hash<std::unique_ptr<T[]>>                    hu1;
  std::hash<typename std::unique_ptr<T[]>::pointer>  hp1;

  VERIFY( hu1(u1) == hp1(u1.get()) );
}

int main()
{
  test01();
  return 0;
}
