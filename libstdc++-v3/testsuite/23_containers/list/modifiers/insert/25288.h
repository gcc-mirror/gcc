// Copyright (C) 2005-2024 Free Software Foundation, Inc.
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

// 23.2.2.3 list modifiers [lib.list.modifiers]

#include <testsuite_hooks.h>
#include <ext/throw_allocator.h>

// libstdc++/25288
template<typename _Tp>
void insert1()
{
  typedef _Tp list_type;
  typedef typename _Tp::value_type value_type;
  typedef typename _Tp::allocator_type allocator_type;
  typedef typename _Tp::size_type size_type;

  for (int j = 0; j < 10; ++j)
    for (int i = 0; i < 10; ++i)
      {
	allocator_type alloc1;
	typename allocator_type::never_adjustor adjust1;
	list_type list1(alloc1);
	
	for (int k = 0; k < j; ++k)
	  list1.push_back(value_type(-(k + 1)));
      
	try
	  {
	    typename allocator_type::always_adjustor adjust2;
	    list1.insert(list1.begin(), 10, 99);
	    VERIFY( false );
	  }
	catch (__gnu_cxx::forced_error&)
	  {
	    VERIFY( true );
	  }
	catch (...)
	  {
	    __throw_exception_again;
	  }
	
	VERIFY( list1.size() == size_type(j) );
	VERIFY( list1.size() == 0 || list1.back() == -j );
	VERIFY( list1.size() == 0 || list1.front() == -1 );

	allocator_type alloc2;
	list_type list2(alloc2);
	
	const int data[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
	
	for (int k = 0; k < j; ++k)
	  list2.push_back(-(k + 1));
	
	try
	  {
	    typename allocator_type::always_adjustor adjust3;
	    list2.insert(list2.begin(), data, data + 10);
	    VERIFY( false );
	  }
	catch (__gnu_cxx::forced_error&)
	  {
	    VERIFY( true );
	  }
	catch (...)
	  {
	    VERIFY( false );
	  }

	VERIFY( list2.size() == size_type(j) );
	VERIFY( list2.size() == 0 || list2.back() == -j );
	VERIFY( list2.size() == 0 || list2.front() == -1 );
      }
}
