// -*- C++ -*- header wrapper.

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
// ISO C++ 14882: 21

#ifndef _CPP_CWCHAR
# define _CPP_CWCHAR 1
# include <bits/std_cstdio.h> 

namespace _C_legacy {
  extern "C" {
#     define _IN_C_LEGACY_
#     pragma GCC system_header
#     include_next <wchar.h>
  }

#if 0
  // XXX
  inline int 
  fwprintf(FILE* __stream, const wchar_t* __format, ...); 

  inline int 
  fwscanf(FILE* __stream, const wchar_t* __format, ...); 

  inline int 
  vfwprintf(FILE* __stream, const wchar_t* __format, va_list __arg); 

  inline int 
  vfwscanf(FILE* __stream, const wchar_t* __format, va_list __arg);

  inline wint_t 
  _CPP_fgetwc_capture(FILE* __stream)
  { return fgetwc(__stream); }

  inline wchar_t*
  _CPP_fgetws_capture(wchar_t* __s, int __n, FILE* __stream)
  { return fgetws(__s, __n, __stream); }

  inline wint_t 
  _CPP_fputwc_capture(wchar_t __c, FILE* __stream)
  { return fputwc(__c, __stream); }

  inline int 
  _CPP_fputws_capture(const wchar_t* __s, FILE* __stream)
  { return fputws(__s, __stream); }

  inline int 
  _CPP_fwide_capture(FILE* __stream, int __mode) 
  { return fwide(__stream, __mode); }

  inline wint_t 
  _CPP_fgetwc_capture(FILE* __stream)
  { return fgetwc(__stream); }

  inline wint_t 
  _CPP_putwc_capture(wchar_t __c, FILE* __stream)
  { return putwc(__c, __stream); }
  
  inline wint_t 
  _CPP_ungetwc_capture(wint_t __c, FILE* __stream)
  { return ungetwc(__c, __stream); }
#endif
} // namespace _C_legacy

# undef wchar_t
# undef wint_t
# undef mbstate_t

# undef fwprintf
# undef fwscanf
# undef swprintf
# undef swscanf
# undef vfwprintf
# undef vfwscanf
# undef vswprintf
# undef vswscanf
# undef vwprintf
# undef vwscanf
# undef wprintf
# undef wscanf
# undef fgetwc
# undef fgetws
# undef fputwc
# undef fputws
# undef fwide
# undef getwc
# undef getwchar
# undef putwc
# undef putwchar
# undef ungetwc
# undef wcstod
# undef wcstof
# undef wcstold
# undef wcstol
# undef wcstoll
# undef wcstoul
# undef wcstoull
# undef wcscpy
# undef wcsncpy
# undef wcscat
# undef wcsncat
# undef wcsmp
# undef wcscoll
# undef wcsncmp
# undef wcsxfrm
# undef wcschr
# undef wcscspn
# undef wcslen
# undef wcspbrk
# undef wcsrchr
# undef wcsspn
# undef wcsstr
# undef wcstok
# undef wmemchr
# undef wmemcmp
# undef wmemcpy
# undef wmemmove
# undef wmemset
# undef wcsftime
# undef btowc
# undef wctob
# undef mbsinit
# undef mbrlen
# undef mbrtowc
# undef wcrtomb
# undef mbsrtowcs
# undef wcsrtombs

namespace std {

  using _C_legacy::wint_t; 
  using _C_legacy::mbstate_t;

#if 0
  using _C_legacy::swprintf;
  using _C_legacy::swscanf;
  using _C_legacy::vswprintf;
  using _C_legacy::vswscanf;
  using _C_legacy::vwprintf;
  using _C_legacy::vwscanf;
  using _C_legacy::wprintf;
  using _C_legacy::wscanf;
  using _C_legacy::getwchar;
  using _C_legacy::putwchar;
#endif

  using _C_legacy::wcstod;
  using _C_legacy::wcstof;
  using _C_legacy::wcstold;
  using _C_legacy::wcstol;
  using _C_legacy::wcstoll;
  using _C_legacy::wcstoul;
  using _C_legacy::wcstoull;
  using _C_legacy::wcscpy;
  using _C_legacy::wcsncpy;
  using _C_legacy::wcscat;
  using _C_legacy::wcsncat;

#if 0
  using _C_legacy::wcsmp;
#endif

  using _C_legacy::wcscoll;
  using _C_legacy::wcsncmp;
  using _C_legacy::wcsxfrm;
  using _C_legacy::wcschr;
  using _C_legacy::wcscspn;
  using _C_legacy::wcslen;
  using _C_legacy::wcspbrk;
  using _C_legacy::wcsrchr;
  using _C_legacy::wcsspn;
  using _C_legacy::wcsstr;
  using _C_legacy::wcstok;
  using _C_legacy::wmemchr;
  using _C_legacy::wmemcmp;
  using _C_legacy::wmemcpy;
  using _C_legacy::wmemmove;
  using _C_legacy::wmemset;

#if 0
  using _C_legacy::wcsftime;
#endif

  using _C_legacy::btowc;
  using _C_legacy::wctob;
  using _C_legacy::mbsinit;
  using _C_legacy::mbrlen;
  using _C_legacy::mbrtowc;
  using _C_legacy::wcrtomb;
  using _C_legacy::mbsrtowcs;
  using _C_legacy::wcsrtombs;

#if 0
  // XXX
  inline int 
  fwprintf(FILE* __stream, const wchar_t* __format, ...); 

  inline int 
  fwscanf(FILE* __stream, const wchar_t* __format, ...); 

  inline int 
  vfwprintf(FILE* __stream, const wchar_t* __format, va_list __arg); 

  inline int 
  vfwscanf(FILE* __stream, const wchar_t* __format, va_list __arg);

  inline wint_t 
  fgetwc(FILE* __stream)
  { return _C_legacy::_CPP_fgetwc_capture(__stream); }

  inline wchar_t*
  fgetws(wchar_t* __s, int __n, FILE* __stream) 
  { return _C_legacy::_CPP_fgetws_capture(__s, __n, __stream); }

  inline wint_t 
  fputwc(wchar_t __c, FILE* __stream)
  { return _C_legacy::_CPP_fputwc_capture(__c, __stream); }

  inline int 
  fputws(const wchar_t* __s, FILE* __stream)
  { return _C_legacy::_CPP_fputws_capture(__s, __stream); }

  inline int 
  fwide(FILE* __stream, int __mode)
  { return _C_legacy::_CPP_fwide_capture(__stream, __mode); }

  inline wint_t 
  getwc(FILE* __stream)
  { return _C_legacy::_CPP_getwc_capture(__stream); }

  inline wint_t 
  putwc(wchar_t __c, FILE* __stream)
  { return _C_legacy::_CPP_putwc_capture(__c, __stream); }
  
  inline wint_t 
  ungetwc(wint_t __c, FILE* __stream)
  { return _C_legacy::_CPP_ungetwc_capture(__c, __stream); }
#endif
}

# undef _IN_C_LEGACY_

#endif





