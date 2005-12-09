// Copyright (C) 2005 Free Software Foundation, Inc.
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
#include <testsuite_allocator.h>

// libstdc++/25288
void test01()
{
  bool test __attribute__((unused)) = true;

  typedef __gnu_test::throw_allocator<int> my_alloc;
  typedef std::list<int, my_alloc > my_list;

  for (int j = 0; j < 10; ++j)
    for (int i = 0; i < 10; ++i)
      {
	my_alloc alloc1(j + i);
	my_list list1(alloc1);
	
	for (int k = 0; k < j; ++k)
	  list1.push_back(-(k + 1));
      
	try
	  {
	    list1.insert(list1.begin(), 10, 99);
	    VERIFY( false );
	  }
	catch (std::bad_alloc&)
	  {
	    VERIFY( true );
	  }
	catch (...)
	  {
	    VERIFY( false );
	  }
	
	VERIFY( list1.size() == my_list::size_type(j) );
	VERIFY( list1.size() == 0 || list1.back() == -j );
	VERIFY( list1.size() == 0 || list1.front() == -1 );

	my_alloc alloc2(j + i);
	my_list list2(alloc2);
	
	const int data[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
	
	for (int k = 0; k < j; ++k)
	  list2.push_back(-(k + 1));
	
	try
	  {
	    list2.insert(list2.begin(), data, data + 10);
	    VERIFY( false );
	  }
	catch (std::bad_alloc&)
	  {
	    VERIFY( true );
	  }
	catch (...)
	  {
	    VERIFY( false );
	  }

	VERIFY( list2.size() == my_list::size_type(j) );
	VERIFY( list2.size() == 0 || list2.back() == -j );
	VERIFY( list2.size() == 0 || list2.front() == -1 );
      }
}

int main()
{
  test01();
  return 0;
}
