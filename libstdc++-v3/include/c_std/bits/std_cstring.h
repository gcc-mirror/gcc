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
  extern "C" void* memcpy(void*, const void*, size_t); 
  extern "C" void* memmove(void*, const void*, size_t); 
  extern "C" char* strcpy(char*, const char*); 
  extern "C" char* strncpy(char*, const char*, size_t); 
  extern "C" char* strcat(char*, const char*); 
  extern "C" char* strncat(char*, const char*, size_t); 
  extern "C" int memcmp(const void*, const void*, size_t); 
  extern "C" int strcmp(const char*, const char*); 
  extern "C" int strcoll(const char*, const char*); 
  extern "C" int strncmp(const char*, const char*, size_t); 
  extern "C" size_t strxfrm(char*, const char*, size_t); 
  extern "C" const void* memchr(const void*, int, size_t); 
  inline void*
  memchr(void* __p, int __c, size_t __n)
  {
    return const_cast<void*>(memchr(const_cast<const void*>(__p), __c, __n));
  }
  extern "C" const char* strchr(const char*, int); 
  inline char*
  strchr(char* __s1, int __n)
  {
    return const_cast<char*>(strchr(const_cast<const char*>(__s1), __n));
  }
  extern "C" size_t strcspn(const char*, const char*); 
  extern "C" const char* strpbrk(const char*, const char*); 
  inline char*
  strpbrk(char* __s1, const char* __s2)
  {
    return const_cast<char*>(strpbrk(const_cast<const char*>(__s1), __s2));
  }
  extern "C" const char* strrchr(const char*, int); 
  inline char*
  strrchr(char* __s1, int __n)
  {
    return const_cast<char*>(strrchr(const_cast<const char*>(__s1), __n));
  }
  extern "C" size_t strspn(const char*, const char*); 
  extern "C" const char* strstr(const char*, const char*); 
  inline char*
  strstr(char* __s1, const char* __s2)
  {
    return const_cast<char*>(strstr(const_cast<const char*>(__s1), __s2));
  }
  extern "C" char* strtok(char*, const char*); 
  extern "C" void* memset(void*, int, size_t); 
  extern "C" char* strerror(int); 
  extern "C" size_t strlen(const char*);
}

#endif



