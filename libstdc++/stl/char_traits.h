/*
 * Copyright (c) 1997
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */ 

#ifndef __SGI_STL_CHAR_TRAITS_H
#define __SGI_STL_CHAR_TRAITS_H

#include <string.h>
#include <wchar.h>

__STL_BEGIN_NAMESPACE

// Class __char_traits_base.

template <class _CharT, class _IntT> struct __char_traits_base {
  typedef _CharT char_type;
  typedef _IntT int_type;
  // typedef streamoff off_type;
  // typedef streampos pos_type;
  // typedef mbstate_t state_type;

  static void assign(char_type& __c1, const char_type& __c2) { __c1 = __c2; }
  static bool eq(const _CharT& __c1, const _CharT& __c2) 
    { return __c1 == __c2; }
  static bool lt(const _CharT& __c1, const _CharT& __c2) 
    { return __c1 < __c2; }

  static int compare(const _CharT* __s1, const _CharT* __s2, size_t __n) {
    for (size_t __i = 0; __i < __n; ++__i)
      if (!eq(__s1[__i], __s2[__i]))
        return __s1[__i] < __s2[__i] ? -1 : 1;
    return 0;
  }

  static size_t length(const _CharT* __s) {
    const _CharT __null = _CharT();
    size_t __i;
    for (__i = 0; !eq(__s[__i], __null); ++__i)
      {}
    return __i;
  }

  static const _CharT* find(const _CharT* __s, size_t __n, const _CharT& __c)
  {
    for ( ; __n > 0 ; ++__s, --__n)
      if (eq(*__s, __c))
        return __s;
    return 0;
  }

  static _CharT* move(_CharT* __s1, const _CharT* __s2, size_t __n) {
    memmove(__s1, __s2, __n * sizeof(_CharT));
    return __s1;
  }
    
  static _CharT* copy(_CharT* __s1, const _CharT* __s2, size_t __n) {
    memcpy(__s1, __s2, __n * sizeof(_CharT));
    return __s1;
  } 

  static _CharT* assign(_CharT* __s, size_t __n, _CharT __c) {
    for (size_t __i = 0; __i < __n; ++__i)
      __s[__i] = __c;
    return __s;
  }

  static int_type not_eof(const int_type& __c) {
    return !eq(__c, eof()) ? __c : 0;
  }

  static char_type to_char_type(const int_type& __c) {
    return static_cast<char_type>(__c);
  }

  static int_type to_int_type(const char_type& __c) {
    return static_cast<int_type>(__c);
  }

  static bool eq_int_type(const int_type& __c1, const int_type& __c2) {
    return __c1 == __c2;
  }

  static int_type eof() {
    return static_cast<int_type>(-1);
  }
};

// Generic char_traits class.  Note that this class is provided only
//  as a base for explicit specialization; it is unlikely to be useful
//  as is for any particular user-defined type.  In particular, it 
//  *will not work* for a non-POD type.

template <class _CharT> struct char_traits
  : public __char_traits_base<_CharT, _CharT>
{};

// Specialization for char.

template<> struct char_traits<char> 
  : public __char_traits_base<char, int>
{
  static int compare(const char* __s1, const char* __s2, size_t __n) 
    { return memcmp(__s1, __s2, __n); }
  
  static size_t length(const char* __s) { return strlen(__s); }

  static void assign(char& __c1, const char& __c2) { __c1 = __c2; }

  static char* assign(char* __s, size_t __n, char __c)
    { memset(__s, __c, __n); return __s; }
};

// Specialization for wchar_t.

template<> struct char_traits<wchar_t>
  : public __char_traits_base<wchar_t, wint_t>
{};

__STL_END_NAMESPACE

#endif /* __SGI_STL_CHAR_TRAITS_H */

// Local Variables:
// mode:C++
// End:

