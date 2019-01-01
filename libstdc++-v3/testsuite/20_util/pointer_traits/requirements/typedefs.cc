// { dg-do compile { target c++11 } }
//
// Copyright (C) 2011-2019 Free Software Foundation, Inc.
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
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 
// NB: This file is for testing memory with NO OTHER INCLUDES.

#include <memory>

template<typename Ptr>
void test01()
{
  // Check for required typedefs
  typedef std::pointer_traits<Ptr>              test_type;
  typedef typename test_type::pointer           pointer;
  typedef typename test_type::element_type      element_type;
  typedef typename test_type::difference_type   difference_type;
  typedef typename test_type::template rebind<char> rebind_type;
}

int main()
{
  test01<int*>();
  test01<void*>();
  test01<std::shared_ptr<int>>();
  test01<std::shared_ptr<void>>();
  test01<std::unique_ptr<int>>();
  test01<std::unique_ptr<void>>();
}
