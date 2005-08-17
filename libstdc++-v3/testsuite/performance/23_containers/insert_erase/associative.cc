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

template<typename Container, int Iter>
  void
  do_loop()
  {
    // XXX
    //    typedef typename Container::value_type test_type;
    typedef int test_type;
    value_type<test_type> test_value;

    const int insert_values = 128; // XXX
    Container obj; // XXX
    for (int i = 0; i < Iter; ++i)
      {
	for (int j = 0; j < insert_values; ++j)
	  obj.insert(++test_value);
      }

    const int erasei = static_cast<int>(Iter / 4);
    for (int i = 0; i < erasei; ++i)
      {
	int key = i * 2;
	obj.erase(key);
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
  typedef associative_containers<int, thread_type>::type container_types;

  typedef test_sequence<thread_type> test_type;
  test_type test("insert_erase_associative");
  __gnu_cxx::apply<test_type, container_types> applier;
  applier(test);

  return 0;
}

