// 2001-06-05 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2025 Free Software Foundation, Inc.
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


// 27.4.2.1.6 class ios_base::init

#include <sstream>
#include <typeinfo>
#include <cstring>
#include <testsuite_hooks.h>

// char_traits specialization
namespace std
{
  template<>
    struct char_traits<unsigned short>
    {
      typedef unsigned short 	char_type;
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

// Non-required instantiations don't have the required facets inbued,
// by default, into the locale object.
// See 27.4.4.1

void test02() 
{
  bool test = true;

  // 02: Calls basic_ios::init, which may call ctype<char_type>...
  try
    {
      std::basic_string<unsigned short>        	str;
      std::basic_ostringstream<unsigned short> 	oss(str);
      
      // Try each member functions for unformatted io.
      // put
      oss.put(324);

      // write
      const unsigned short us[4] = {1246, 433, 520, 0};
      oss.write(us, 4);

      // flush
      oss.flush();
    }
  catch(const std::bad_cast& obj)
    {
      // Should be able to do the above without calling fill() and
      // forcing a call to widen...
      test = false;
    }
  catch(...)
    {
      test = false;
    }
  VERIFY( test );
}

#if !__GXX_WEAK__
// Explicitly instantiate for systems with no COMDAT or weak support.
template 
  const std::basic_string<unsigned short>::size_type 
  std::basic_string<unsigned short>::_Rep::_S_max_size;

template 
  const unsigned short
  std::basic_string<unsigned short>::_Rep::_S_terminal;
#endif

int main()
{
  test02();
  return 0;
}
