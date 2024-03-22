// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

// Copyright (C) 2011-2024 Free Software Foundation, Inc.
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

#include <tuple>
#include <type_traits>
#include <memory>
#include <testsuite_hooks.h>

template<typename T>
  typename std::decay<T>::type copy(T&& x)
  { return std::forward<T>(x); }

// libstdc++/48476
void test01()
{
  std::shared_ptr<int> p(new int()), q, r;
  
  std::tuple<std::shared_ptr<int>&, int>  t0(p, 23), t1(q, 0);
  t1 = copy(t0);  // shall be equivalent to
                  // q = p; std::get<1>(t1) = std::get<1>(t0);
  VERIFY( q == p ); 

  std::tuple<std::shared_ptr<int>&, char> t2(r, 0);
  t2 = copy(t1);  // shall be equivalent to
                  // r = q; std::get<1>(t2) = std::get<1>(t1);
  VERIFY( r == q );
}

int main()
{
  test01();
  return 0;
}
