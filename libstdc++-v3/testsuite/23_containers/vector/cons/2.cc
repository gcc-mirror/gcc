// 1999-06-29 bkoz

// Copyright (C) 1999-2001, 2002, 2003, 2004 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 23.2.4.1 vector constructors, copy, and assignment

#include <vector>
#include <string>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>
 
template<typename T>
  struct A { };

struct B { };

// 2
template class std::vector<double>;
template class std::vector< A<B> >;

// libstdc++/102
void test02()
{
  std::vector<int> v1;
  std::vector<int> v2(v1);
}

#if !__GXX_WEAK__ && _MT_ALLOCATOR_H
// Explicitly instantiate for systems with no COMDAT or weak support.
template class __gnu_cxx::__mt_alloc<int>;
template class __gnu_cxx::__mt_alloc<double>;
template class __gnu_cxx::__mt_alloc<A<B> >;
#endif

int main()
{
  test02(); 
  return 0;
}
