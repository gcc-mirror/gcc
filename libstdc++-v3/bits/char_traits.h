// Character Traits for use by standard string and iostream -*- C++ -*-

// Copyright (C) 1997-1999, 2000 Free Software Foundation, Inc.
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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

//
// ISO C++ 14882: 21  Strings library
//

#ifndef _CPP_BITS_CHAR_TRAITS_H
#define _CPP_BITS_CHAR_TRAITS_H 1

#include <bits/std_cwchar.h> 	// For mbstate_t.
#include <bits/std_cstring.h> 	// For memmove, memset, memchr
#include <bits/fpos.h> 		// For streamoff, streamsize

namespace std {

  // Same as iosfwd
#ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
  // Can't have self-recursive types for streampos. 
  // 21.1.3.1 char_traits sets size_type to streampos
  // 27.4.1 
  // And here, where streampos is typedefed to fpos<traits::state_type>
    typedef fpos<mbstate_t> 	streampos;
#  ifdef _GLIBCPP_USE_WCHAR_T
    typedef fpos<mbstate_t> 	wstreampos;
#  endif
#endif

  // 21.1.2 Basis for explicit _Traits specialization 
  // NB: That for any given actual character type this definition is
  // probably wrong.

  template<class _CharT>
    struct char_traits
    {
      typedef _CharT 		char_type;
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
	  if (!eq(__s1[__i],__s2[__i]))
	    return lt(__s1[__i],__s2[__i]) ? -1 : 1;
	return 0; 
      }

      static size_t
      length(const char_type* __s)
      { 
	const char_type* __p = __s; 
	while (*__p) ++__p; 
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
      to_char_type(const int_type& __c)
      { return char_type(__c); }

      static int_type 
      to_int_type(const char_type& __c) { return int_type(__c); }

      static bool 
      eq_int_type(const int_type& __c1, const int_type& __c2)
      { return __c1 == __c2; }

      static state_type 
      get_state (pos_type __pos) { return __pos.state(); }

      static int_type 
      eof() { return static_cast<int_type>(-1); }

      static int_type 
      eos() { return int_type(); }

      static int_type 
      not_eof(const int_type& __c)
      { return eq_int_type(__c, eof()) ? int_type(0) : __c; }
    };

  // 21.1.4  char_traits specializations
  template<>
    struct char_traits<char>
    {
      typedef char 		char_type;
      typedef unsigned int 	int_type;
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
      { return memcmp(__s1, __s2, __n); }

      static size_t
      length(const char_type* __s)
      { return strlen(__s); }

      static const char_type* 
      find(const char_type* __s, size_t __n, const char_type& __a)
      { return static_cast<char*>(memchr(__s, __a, __n)); }

      static char_type* 
      move(char_type* __s1, const char_type* __s2, size_t __n)
      { return static_cast<char*>(memmove(__s1, __s2, __n)); }

      static char_type* 
      copy(char_type* __s1, const char_type* __s2, size_t __n)
      {  return static_cast<char*>(memcpy(__s1, __s2, __n)); }

      static char_type* 
      assign(char_type* __s, size_t __n, char_type __a)
      { return static_cast<char*>(memset(__s, __a, __n)); }

      static char_type 
      to_char_type(const int_type& __c)
      { return static_cast<char>(__c); }

      //  To keep both the byte 0xff and the eof symbol 0xffffffff
      //  from ending up as 0xffffffff.
      static int_type 
      to_int_type(const char_type& __c)
      { return static_cast<int_type>(static_cast<unsigned char>(__c)); }

      static bool 
      eq_int_type(const int_type& __c1, const int_type& __c2)
      { return __c1 == __c2; }

      static state_type 
      get_state(pos_type __pos) { return __pos.state(); }

      static int_type 
      eof() { return static_cast<int_type>(EOF); }

      static int_type 
      eos() { return '\0'; }

      static int_type 
      not_eof(const int_type& __c)
      { return (__c == eof()) ? 0 : __c; }
  };


#ifdef _GLIBCPP_USE_WCHAR_T
  template<>
    struct char_traits<wchar_t>
    {
      typedef wchar_t 		char_type;
      typedef wint_t 		int_type;
      typedef wstreamoff 	off_type;
      typedef wstreampos 	pos_type;
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
	for (int_type __i = 0; __i < __n; ++__i)
	  if (!eq(__s1[__i], __s2[__i]))
	    return lt(__s1[__i], __s2[__i]) ? -1 : 1;
	return 0; 
      }

      static size_t
      length(const char_type* __s)
      { 
	const char_type* __p = __s; 
	while (*__p) 
	  ++__p; 
	return (__p - __s); 
      }

      static const char_type* 
      find(const char_type* __s, size_t __n, const char_type& __a)
      { 
	for (const char_type* __p = __s; size_t(__p - __s) < __n; ++__p)
	  if (*__p == __a) 
	    return __p;
	return 0;
      }

      static char_type* 
      move(char_type* __s1, const char_type* __s2, int_type __n)
      { return static_cast<wchar_t*>(memmove(__s1, __s2, 
					     __n * sizeof(wchar_t))); }

      static char_type* 
      copy(char_type* __s1, const char_type* __s2, size_t __n)
      { return static_cast<wchar_t*>(memcpy(__s1, __s2, 
					    __n * sizeof(wchar_t))); }

      static char_type* 
      assign(char_type* __s, size_t __n, char_type __a)
      { 
	for (char_type* __p = __s; __p < __s + __n; ++__p) 
	  assign(*__p, __a);
        return __s; 
      }

      static char_type 
      to_char_type(const int_type& __c) { return char_type(__c); }

      static int_type 
      to_int_type(const char_type& __c) { return int_type(__c); }

      static bool 
      eq_int_type(const int_type& __c1, const int_type& __c2)
      { return __c1 == __c2; }

      static state_type 
      get_state(pos_type __pos) { return __pos.state(); }

      static int_type 
      eof() { return static_cast<int_type>(WEOF); }

      static int_type 
      eos() { return int_type(); }

      static int_type 
      not_eof(const int_type& __c)
      { return eq_int_type(__c, eof()) ? 0 : __c; }
  };
#endif //_GLIBCPP_USE_WCHAR_T

  template<typename _CharT, typename _Traits>
    struct _Char_traits_match
    {
      _CharT _M_c;
      _Char_traits_match(_CharT const& __c) : _M_c(__c) { }

      bool 
      operator()(_CharT const& __a) { return _Traits::eq(_M_c,__a); }
    };

} // namespace std


#endif /* _CPP_BITS_CHAR_TRAITS_H */

