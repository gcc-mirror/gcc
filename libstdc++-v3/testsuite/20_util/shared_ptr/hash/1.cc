// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

// 2010-06-11  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2023 Free Software Foundation, Inc.
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

  std::shared_ptr<T>                s0(new T);
  std::hash<std::shared_ptr<T>>     hs0;
  std::hash<T*>                     hp0;

  VERIFY( hs0(s0) == hp0(s0.get()) );

  std::__shared_ptr<T>              s1(new T);
  std::hash<std::__shared_ptr<T>>   hs1;
  std::hash<T*>                     hp1;

  VERIFY( hs1(s1) == hp1(s1.get()) );
}

int main()
{
  test01();
  return 0;
}
