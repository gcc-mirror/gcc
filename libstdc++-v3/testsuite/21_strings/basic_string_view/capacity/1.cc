// { dg-do run { target c++17 } }

// Copyright (C) 2013-2021 Free Software Foundation, Inc.
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

// string_view size, length

#include <string_view>
#include <cstring>
#include <testsuite_hooks.h>

template<typename T>
  struct A { };

template<typename T>
  bool
  operator==(const A<T>&, const A<T>&) { return true; }

template<typename T>
  bool
  operator<(const A<T>&, const A<T>&) { return true; }

struct B { };

// char_traits specialization
namespace std
{
  template<>
    struct char_traits<A<B> >
    {
      typedef A<B> 		char_type;
      // Unsigned as wint_t in unsigned.
      typedef unsigned long  	int_type;
      typedef streampos 	pos_type;
      typedef streamoff 	off_type;
      typedef mbstate_t 	state_type;
      
      static void 
      assign(char_type& __c1, const char_type& __c2)
      { __c1 = __c2; }

      static bool 
      eq(const char_type& __c1, const char_type& __c2)
      { return __c1 == __c2; }

      static bool 
      lt(const char_type& __c1, const char_type& __c2)
      { return __c1 < __c2; }

      static int 
      compare(const char_type* __s1, const char_type* __s2, size_t __n)
      { 
	for (size_t __i = 0; __i < __n; ++__i)
	  if (!eq(__s1[__i], __s2[__i]))
	    return lt(__s1[__i], __s2[__i]) ? -1 : 1;
	return 0; 
      }

      static size_t
      length(const char_type* __s)
      { 
	const char_type* __p = __s; 
	while (__p) 
	  ++__p; 
	return (__p - __s); 
      }

      static const char_type* 
      find(const char_type* __s, size_t __n, const char_type& __a)
      { 
	for (const char_type* __p = __s; size_t(__p - __s) < __n; ++__p)
	  if (*__p == __a) return __p;
	return 0;
      }

      static char_type* 
      move(char_type* __s1, const char_type* __s2, size_t __n)
      { return (char_type*) memmove(__s1, __s2, __n * sizeof(char_type)); }

      static char_type* 
      copy(char_type* __s1, const char_type* __s2, size_t __n)
      { return (char_type*) memcpy(__s1, __s2, __n * sizeof(char_type)); }

      static char_type* 
      assign(char_type* __s, size_t __n, char_type __a)
      { 
	for (char_type* __p = __s; __p < __s + __n; ++__p) 
	  assign(*__p, __a);
        return __s; 
      }

      static char_type 
      to_char_type(const int_type&)
      { return char_type(); }

      static int_type 
      to_int_type(const char_type&) { return int_type(); }

      static bool 
      eq_int_type(const int_type& __c1, const int_type& __c2)
      { return __c1 == __c2; }

      static int_type 
      eof() { return static_cast<int_type>(-1); }

      static int_type 
      not_eof(const int_type& __c)
      { return eq_int_type(__c, eof()) ? int_type(0) : __c; }
    };
} // namespace std

void
test01()
{
  std::basic_string_view<A<B>> str02;
  typedef std::basic_string_view< A<B> >::size_type size_type_o;
  size_type_o sz03;
  size_type_o sz04;

  // non-POD types: size, length, max_size, empty()
  bool b01 = str02.empty();  
  VERIFY( b01 == true );
  sz03 = str02.size();
  sz04 = str02.length();
  VERIFY( sz03 == sz04 );
  str02.data();
  sz03 = str02.size();
  sz04 = str02.length();
  VERIFY( sz03 == sz04 );

  sz03 = str02.max_size();  
  VERIFY( sz03 >= sz04 );

  sz03 = str02.size();
  str02 = {};
  b01 = str02.empty(); 
  VERIFY( b01 == true );
  sz04 = str02.size();  
  VERIFY( sz03 >= sz04 );
}

int
main()
{
  test01();

  return 0;
}
