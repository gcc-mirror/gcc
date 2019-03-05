// 1999-05-11 bkoz

// Copyright (C) 1999-2019 Free Software Foundation, Inc.
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

// 21.3.3 string capacity

#include <string>
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

void test01()
{
  // non POD types : resize, capacity, reserve
  std::basic_string< A<B> > str02;
  typedef std::basic_string< A<B> >::size_type size_type_o;
  size_type_o sz03;
  size_type_o sz04;

  sz03 = str02.capacity();
  str02.reserve(100);
  sz04 = str02.capacity();
  VERIFY( sz04 >= sz03 );
  VERIFY( sz04 >= 100 );
  str02.reserve();
  sz03 = str02.capacity();
#if _GLIBCXX_USE_CXX11_ABI
  VERIFY( sz03 < 100);
#else
  VERIFY( sz03 == 0 );
#endif

  sz03 = str02.size() + 5;
  str02.resize(sz03);
  sz04 = str02.size();
  VERIFY( sz03 == sz04 );

  sz03 = str02.size() - 5;
  str02.resize(sz03);
  sz04 = str02.size();
  VERIFY( sz03 == sz04 );

  A<B> inst_obj;
  std::basic_string<A<B> > str07(30, inst_obj);
  std::basic_string<A<B> > str08 = str07;
  str07 = str08 + str07;
  VERIFY( str07.capacity() >= str07.size() );
  VERIFY( str08.capacity() >= str08.size() );

  // non-POD types: size, length, max_size, clear(), empty()
  bool b01 = str02.empty();  
  VERIFY( b01 == true );
  sz03 = str02.size();
  sz04 = str02.length();
  VERIFY( sz03 == sz04 );
  str02.c_str();
  sz03 = str02.size();
  sz04 = str02.length();
  VERIFY( sz03 == sz04 );

  sz03 = str02.max_size();  
  VERIFY( sz03 >= sz04 );

  sz03 = str02.size();
  str02.clear();  
  b01 = str02.empty(); 
  VERIFY( b01 == true );
  sz04 = str02.size();  
  VERIFY( sz03 >= sz04 );
}

#if !__GXX_WEAK__
// Explicitly instantiate for systems with no COMDAT or weak support.
template 
  const std::basic_string< A<B> >::size_type 
  std::basic_string< A<B> >::_Rep::_S_max_size;

template 
  const A<B>
  std::basic_string< A<B> >::_Rep::_S_terminal;
#endif

int main()
{
  test01();
  return 0;
}
