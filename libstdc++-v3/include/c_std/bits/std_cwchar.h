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
// ISO C++ 14882: ???
//

// Note: This is not a conforming implementation.

#ifndef _CPP_CWCHAR
#define _CPP_CWCHAR 1

#include <bits/c++config.h>
#include <bits/std_cstddef.h>
#include <bits/std_cstdio.h>
#include <bits/std_cstdarg.h>

#if _GLIBCPP_HAVE_WCHAR_H
#pragma GCC system_header
#include <wchar.h>
#endif

// Need to do a bit of trickery here with mbstate_t as char_traits
// assumes it is in wchar.h, regardless of wchar_t specializations.
#ifndef _GLIBCPP_HAVE_MBSTATE_T
extern "C" 
{
  typedef struct 
  {
    int __fill[6];
  } mbstate_t;
}
#endif

namespace std 
{
  using ::mbstate_t;
}

// Get rid of those macros defined in <wchar.h> in lieu of real functions.
#undef vwprintf
#undef vwscanf
#undef wprintf
#undef wscanf
#undef getwc
#undef getwchar
#undef mbsinit
#undef mbrlen
#undef mbrtowc
#undef mbsrtowcs
#undef wcsrtombs
#undef putwc
#undef putwchar
#undef ungetwc
#undef wcrtomb
#undef wcstod
#undef wcstof
#undef wcstol
#undef wcstoul
#undef wcscpy
#undef wcsncpy
#undef wcscat
#undef wcsncat
#undef wcscmp
#undef wcscoll
#undef wcsncmp
#undef wcsxfrm
#undef wcschr
#undef wcscspn
#undef wcslen
#undef wcspbrk
#undef wcsrchr
#undef wcsspn
#undef wcsstr
#undef wcstok
#undef wmemchr
#undef wmemcmp
#undef wmemcpy
#undef wmemmove
#undef wmemset
#undef wcsftime
#undef wcstold
#undef wcstoll
#undef wcstoull

#if _GLIBCPP_USE_WCHAR_T
namespace std
{
  using ::wint_t;

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
  extern "C" const wchar_t* wcschr(const wchar_t*, wchar_t); 
  inline wchar_t*
  wcschr(wchar_t* __p, wchar_t __c)
  {
    return const_cast<wchar_t*>(wcschr(const_cast<const wchar_t*>(__p), __c));
  }
  extern "C" size_t wcscspn(const wchar_t*, const wchar_t*); 
  extern "C" size_t wcslen(const wchar_t*); 
  extern "C" const wchar_t* wcspbrk(const wchar_t*, const wchar_t*); 
  inline wchar_t*
  wcspbrk(wchar_t* __s1, wchar_t* __s2)
  {
    return const_cast<wchar_t*>(wcspbrk(const_cast<const wchar_t*>(__s1), __s2));
  }
  extern "C" const wchar_t* wcsrchr(const wchar_t*, wchar_t); 
  inline wchar_t*
  wcsrchr(wchar_t* __p, wchar_t __c)
  {
    return const_cast<wchar_t*>(wcsrchr(const_cast<const wchar_t*>(__p), __c));
  }
  extern "C" size_t wcsspn(const wchar_t*, const wchar_t*); 
  extern "C" const wchar_t* wcsstr(const wchar_t*, const wchar_t*); 
  inline wchar_t*
  wcsstr(wchar_t* __s1, wchar_t* __s2)
  {
    return const_cast<wchar_t*>(wcsstr(const_cast<const wchar_t*>(__s1), __s2));
  }
  extern "C" wchar_t* wcstok(wchar_t*, const wchar_t*, wchar_t**); 
  extern "C" const wchar_t* wmemchr(const wchar_t*, wchar_t, size_t);
  inline wchar_t*
  wmemchr(wchar_t* __p, wchar_t __c, size_t __n)
  {
    return const_cast<wchar_t*>(wmemchr(const_cast<const wchar_t*>(__p), __c, __n));
  }
  extern "C" int wmemcmp(const wchar_t*, const wchar_t*, size_t); 
  extern "C" wchar_t* wmemcpy(wchar_t*, const wchar_t*, size_t); 
  extern "C" wchar_t* wmemmove(wchar_t*, const wchar_t*, size_t); 
  extern "C" wchar_t* wmemset(wchar_t*, wchar_t, size_t); 
  extern "C" size_t wcsftime(wchar_t*, size_t, const wchar_t*, const struct tm*); 
}

#if _GLIBCPP_USE_C99
namespace c99
{
  extern "C" long double wcstold(const wchar_t*, wchar_t**); 
  extern "C" long long int wcstoll(const wchar_t*, wchar_t**, int); 
  extern "C" unsigned long long int wcstoull(const wchar_t*, wchar_t**, int); 
}

namespace std
{
  using c99::wcstold;
  using c99::wcstoll;
  using c99::wcstoull;
}
#endif

#endif //_GLIBCPP_USE_WCHAR_T

#endif 
