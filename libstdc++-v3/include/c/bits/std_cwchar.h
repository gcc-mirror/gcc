// -*- C++ -*- forwarding header.

// Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
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
// ISO C++ 14882: ???
//

// Note: This is not a conforming implementation.

#ifndef _CPP_CWCHAR
#define _CPP_CWCHAR 1

#include <bits/c++config.h>
#include <bits/std_cstdio.h>
#include <bits/std_cstdarg.h>

#if _GLIBCPP_USE_WCHAR_T
#pragma GCC system_header
#include_next <wchar.h>

// Get rid of those macros defined in <wchar.h> in lieu of real functions.
#undef getwchar

namespace std
{
  using ::wint_t;
  using ::mbstate_t;

  extern "C" wint_t btowc(int); 
  extern "C" int wctob(wint_t); 
  extern "C" wint_t fgetwc(FILE*); 
  extern "C" wchar_t* fgetws(wchar_t*, int, FILE*); 
  extern "C" wint_t fputwc(wchar_t, FILE*); 
  extern "C" int fputws(const wchar_t*, FILE*); 
  extern "C" int fwide(FILE*, int); 
  extern "C" int fwprintf(FILE*, const wchar_t*, ...); 
  extern "C" int fwscanf(FILE*, const wchar_t*, ...); 
  extern "C" int swprintf(wchar_t*, size_t, const wchar_t*, ...); 
  extern "C" int swscanf(const wchar_t*, const wchar_t*, ...); 
  extern "C" int vfwprintf(FILE*, const wchar_t*, va_list); 
  extern "C" int vfwscanf(FILE*, const wchar_t*, va_list); 
  extern "C" int vswprintf(wchar_t*, size_t, const wchar_t*, va_list); 
  extern "C" int vswscanf(const wchar_t*, const wchar_t*, va_list); 
  extern "C" int vwprintf(const wchar_t*, va_list); 
  extern "C" int vwscanf(const wchar_t*, va_list); 
  extern "C" int wprintf(const wchar_t*, ...); 
  extern "C" int wscanf(const wchar_t*, ...); 
  extern "C" wint_t getwc(FILE* stream); 
  extern "C" wint_t getwchar(void); 
  extern "C" int mbsinit(const mbstate_t*); 
  extern "C" size_t mbrlen(const char*, size_t, mbstate_t*); 
  extern "C" size_t mbrtowc(wchar_t*, const char*, size_t, mbstate_t*); 
  extern "C" size_t mbsrtowcs(wchar_t*, const char**, size_t, mbstate_t*); 
  extern "C" size_t wcsrtombs(char*, const wchar_t **, size_t, mbstate_t*);
  extern "C" wint_t putwc(wchar_t, FILE*); 
  extern "C" wint_t putwchar(wchar_t); 
  extern "C" wint_t ungetwc(wint_t, FILE*);
  extern "C" size_t wcrtomb(char*, wchar_t, mbstate_t*); 
  extern "C" double wcstod(const wchar_t*, wchar_t**); 
  extern "C" float wcstof(const wchar_t*, wchar_t**); 
  extern "C" long int wcstol(const wchar_t*, wchar_t**, int); 
  extern "C" unsigned long int wcstoul(const wchar_t*, wchar_t**, int); 
  extern "C" wchar_t* wcscpy(wchar_t* s1, const wchar_t*); 
  extern "C" wchar_t* wcsncpy(wchar_t*, const wchar_t*, size_t); 
  extern "C" wchar_t* wcscat(wchar_t*, const wchar_t*); 
  extern "C" wchar_t* wcsncat(wchar_t*, const wchar_t*, size_t); 
  extern "C" int wcscmp(const wchar_t*, const wchar_t*); 
  extern "C" int wcscoll(const wchar_t*, const wchar_t*); 
  extern "C" int wcsncmp(const wchar_t*, const wchar_t*, size_t); 
  extern "C" size_t wcsxfrm(wchar_t*, const wchar_t*, size_t); 
  extern "C" wchar_t* wcschr(const wchar_t*, wchar_t); 
  extern "C" size_t wcscspn(const wchar_t*, const wchar_t*); 
  extern "C" size_t wcslen(const wchar_t*); 
  extern "C" wchar_t* wcspbrk(const wchar_t*, const wchar_t*); 
  extern "C" wchar_t* wcsrchr(const wchar_t*, wchar_t); 
  extern "C" size_t wcsspn(const wchar_t*, const wchar_t*); 
  extern "C" wchar_t* wcsstr(const wchar_t*, const wchar_t*); 
  extern "C" wchar_t* wcstok(wchar_t*, const wchar_t*, wchar_t**); 
  extern "C" wchar_t* wmemchr(const wchar_t*, wchar_t, size_t);
  extern "C" int wmemcmp(const wchar_t*, const wchar_t*, size_t); 
  //extern "C" int wmemcmp(wchar_t*, const wchar_t*, size_t); 
  extern "C" wchar_t* wmemcpy(wchar_t*, const wchar_t*, size_t); 
  extern "C" wchar_t* wmemmove(wchar_t*, const wchar_t*, size_t); 
  extern "C" wchar_t* wmemset(wchar_t*, wchar_t, size_t); 
  extern "C" size_t wcsftime(wchar_t*, size_t, const wchar_t*, const struct tm*); 

#if 0
  // Full C99 listing
  extern "C" long double wcstold(const wchar_t*, wchar_t**); 
  extern "C" long long int wcstoll(const wchar_t*, wchar_t**, int); 
  extern "C" unsigned long long int wcstoull(const wchar_t*, wchar_t**, int); 
#endif
}

#else
extern "C" 
{
  typedef struct 
  {
    int __fill[6];
  } mbstate_t;
}

namespace std 
{
  using ::mbstate_t;
}
#endif //_GLIBCPP_USE_WCHAR_T


#endif 




















