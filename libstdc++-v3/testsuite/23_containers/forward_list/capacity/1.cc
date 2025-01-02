// { dg-do run { target c++11 } }

// Copyright (C) 2008-2025 Free Software Foundation, Inc.
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

// 23.2.3.n forward_list capacity [lib.forward_list.capacity]

#include <forward_list>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

void
test01()
{
  std::forward_list<double> fld;
  VERIFY(fld.empty() == true);

  fld.push_front(1.0);
  VERIFY(fld.empty() == false);

  fld.resize(0);
  VERIFY(fld.empty() == true);

#ifdef _GLIBCXX_DEBUG
  namespace C = std::_GLIBCXX_STD_C;
#else
  namespace C = std;
#endif

  std::allocator<C::_Fwd_list_node<double>> a;
  VERIFY( fld.max_size() == __gnu_test::max_size(a) );

#if _GLIBCXX_FWDLIST_USE_ALLOC_PTR
  std::allocator<C::__fwdlist::_Node<double*>> b;
  VERIFY( __gnu_test::max_size(b) == __gnu_test::max_size(a) );
#endif
}

int
main()
{
  test01();
  return 0;
}
