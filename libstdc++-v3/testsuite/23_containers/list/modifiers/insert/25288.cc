// Copyright (C) 2005, 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without Pred the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 23.2.2.3 list modifiers [lib.list.modifiers]

#include <list>
#include <testsuite_hooks.h>
#include <ext/throw_allocator.h>

// libstdc++/25288
void test01()
{
  bool test __attribute__((unused)) = true;

  typedef int value_type;
  typedef __gnu_cxx::throw_allocator<value_type> allocator_type;
  typedef std::list<value_type, allocator_type> list_type;

  for (int j = 0; j < 10; ++j)
    for (int i = 0; i < 10; ++i)
      {
	allocator_type alloc1;
	allocator_type::zero_throw_prob_adjustor adjust1;
	list_type list1(alloc1);
	
	for (int k = 0; k < j; ++k)
	  list1.push_back(value_type(-(k + 1)));
      
	try
	  {
	    alloc1.set_throw_prob(1);
	    list1.insert(list1.begin(), 10, 99);
	    VERIFY( false );
	  }
	catch (__gnu_cxx::forced_exception_error&)
	  {
	    VERIFY( true );
	  }
	catch (...)
	  {
	    VERIFY( false );
	  }
	
	VERIFY( list1.size() == list_type::size_type(j) );
	VERIFY( list1.size() == 0 || list1.back() == -j );
	VERIFY( list1.size() == 0 || list1.front() == -1 );

	allocator_type alloc2;
	allocator_type::zero_throw_prob_adjustor adjust2;
	list_type list2(alloc2);
	
	const int data[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
	
	for (int k = 0; k < j; ++k)
	  list2.push_back(-(k + 1));
	
	try
	  {
	    alloc2.set_throw_prob(1);
	    list2.insert(list2.begin(), data, data + 10);
	    VERIFY( false );
	  }
	catch (__gnu_cxx::forced_exception_error&)
	  {
	    VERIFY( true );
	  }
	catch (...)
	  {
	    VERIFY( false );
	  }

	VERIFY( list2.size() == list_type::size_type(j) );
	VERIFY( list2.size() == 0 || list2.back() == -j );
	VERIFY( list2.size() == 0 || list2.front() == -1 );
      }
}

int main()
{
  test01();
  return 0;
}
