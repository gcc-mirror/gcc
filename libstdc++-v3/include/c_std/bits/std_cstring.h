// -*- C++ -*- forwarding header.

// Copyright (C) 1997, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
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
// ISO C++ 14882: 20.4.6  C library
//

// Note: This is not a conforming implementation.

#ifndef _CPP_CSTRING
#define _CPP_CSTRING 1

#include <bits/c++config.h>
#include <bits/std_cstddef.h>


// Need to mangle these "C" functions because C++ modifies their signature.
#define memcpy __glibcpp_memcpy
#define memmove __glibcpp_memmove
#define strcpy __glibcpp_strcpy
#define strncpy __glibcpp_strncpy
#define strcat __glibcpp_strcat
#define strncat __glibcpp_strncat
#define memcmp __glibcpp_memcmp
#define strcmp __glibcpp_strcmp
#define strcoll __glibcpp_strcoll
#define strncmp __glibcpp_strncmp
#define strxfrm __glibcpp_strxfrm
#define memchr __glibcpp_memchr
#define strchr __glibcpp_strchr
#define strcspn __glibcpp_strcspn
#define strpbrk __glibcpp_strpbrk
#define strrchr __glibcpp_strrchr
#define strspn __glibcpp_strspn
#define strstr __glibcpp_strstr
#define strtok __glibcpp_strtok
#define memset __glibcpp_memset
#define strerror __glibcpp_strerror
#define strlen __glibcpp_strlen

#pragma GCC system_header
#include <string.h>

// Get rid of those macros defined in <string.h> in lieu of real functions.
#undef memcpy
#undef memmove
#undef strcpy
#undef strncpy
#undef strcat
#undef strncat
#undef memcmp
#undef strcmp
#undef strcoll
#undef strncmp
#undef strxfrm
#undef memchr
#undef strchr
#undef strcspn
#undef strpbrk
#undef strrchr
#undef strspn
#undef strstr
#undef strtok
#undef memset
#undef strerror
#undef strlen

namespace std 
{
  inline void*
  memcpy(void* __p1, const void* __p2, size_t __n)
  { return __builtin_memcpy(__p1, __p2, __n); }

  extern "C" void* memmove(void*, const void*, size_t); 

  inline char*
  strcpy(char* __s1, const char* __s2)
  { return __builtin_strcpy(__s1, __s2); }

  inline char*
  strncpy(char* __s1, const char* __s2, size_t __n)
  { return __builtin_strncpy(__s1, __s2, __n); }

  inline char*
  strcat(char* __s1, const char* __s2)
  { return __builtin_strcat(__s1, __s2); }

  inline char*
  strncat(char* __s1, const char* __s2, size_t __n)
  { return __builtin_strncat(__s1, __s2, __n); }

  inline int
  memcmp(const void* __p1, const void* __p2, size_t __n)
  { return __builtin_memcmp(__p1, __p2, __n); }

  inline int
  strcmp(const char* __s1, const char* __s2)
  { return __builtin_strcmp(__s1, __s2); }

  extern "C" int strcoll(const char*, const char*); 

  inline int
  strncmp(const char* __s1, const char* __s2, size_t __n)
  { return __builtin_strncmp(__s1, __s2, __n); }

  extern "C" size_t strxfrm(char*, const char*, size_t); 
  extern "C" const void* memchr(const void*, int, size_t); 

  inline void*
  memchr(void* __p, int __c, size_t __n)
  {
    return const_cast<void*>(memchr(const_cast<const void*>(__p), __c, __n));
  }

  inline const char*
  strchr(const char* __s1, int __n)
  { return const_cast<const char*>(__builtin_strchr(__s1, __n)); }

  inline char*
  strchr(char* __s1, int __n)
  {
    return 
      const_cast<char*>(__builtin_strchr(const_cast<const char*>(__s1), __n));
  }

  inline size_t
  strcspn(const char* __s1, const char* __s2)
  { return __builtin_strcspn(__s1, __s2); }

  inline const char*
  strpbrk(const char* __s1, const char* __s2)
  { return const_cast<char*>(__builtin_strpbrk(__s1, __s2)); }

  inline char*
  strpbrk(char* __s1, const char* __s2)
  {
    return const_cast<char*>
      (__builtin_strpbrk(const_cast<const char*>(__s1), __s2));
  }

  inline const char*
  strrchr(const char* __s1, int __n)
  { return const_cast<char*>(__builtin_strrchr(__s1, __n)); }

  inline char*
  strrchr(char* __s1, int __n)
  { return __builtin_strrchr(const_cast<const char*>(__s1), __n); }

  inline size_t
  strspn(const char* __s1, const char* __s2)
  { return __builtin_strspn(__s1, __s2); }

  inline const char*
  strstr(const char* __s1, const char* __s2)
  { return const_cast<char*>(__builtin_strstr (__s1, __s2)); }

  inline char*
  strstr(char* __s1, const char* __s2)
  {
    return (const_cast<char*>
	    (__builtin_strstr(const_cast<const char*>(__s1), __s2)));
  }

  extern "C" char* strtok(char*, const char*); 

  inline void*
  memset(void* __p, int __c, size_t __n)
  { return __builtin_memset(__p, __c, __n); }

  extern "C" char* strerror(int); 

  inline size_t
  strlen(const char* __s)
  { return __builtin_strlen(__s); }
}

#endif



