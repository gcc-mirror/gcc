// Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <testsuite_common_types.h>

// libstdc++/13823 recast for this testing framework
template<typename Container, int Iter>
  void
  do_loop()
  {
    try
      {
	Container obj;
	for (int c = 0; c < 100; c++)
	  {
	    for (unsigned i = 0; i < Iter; ++i) 
	      obj[i] = i;
	  }
      }
    catch(...)
      {
	// No point allocating all available memory, repeatedly.	
      }
  }

int
main()
{ 
#ifdef TEST_S1
#define thread_type false
#endif    

#ifdef TEST_T1
#define thread_type true
#endif    

  using __gnu_test::associative_containers;
  typedef __gnu_test::maps<int, thread_type>::type container_types;

  typedef test_sequence<thread_type> test_type;
  test_type test("index_associative");
  __gnu_cxx::apply<test_type, container_types> applier;
  applier(test);

  return 0;
}

