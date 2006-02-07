// Locale support -*- C++ -*-

// Copyright (C) 1999, 2000, 2001, 2002, 2003, 2005, 2006
// Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
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
// ISO C++ 14882: 22.1  Locales
//

#include <locale>

_GLIBCXX_BEGIN_NAMESPACE(std)

  template
    int
    __convert_from_v(char*, const int, const char*, double, 
		     const __c_locale&, int);

  template
    int
    __convert_from_v(char*, const int, const char*, long double, 
		     const __c_locale&, int);

_GLIBCXX_END_NAMESPACE

// XXX GLIBCXX_ABI Deprecated
#if defined _GLIBCXX_LONG_DOUBLE_COMPAT

#define _GLIBCXX_LDBL_COMPAT(dbl, ldbl) \
  extern "C" void ldbl (void) __attribute__ ((alias (#dbl), weak))

# if _GLIBCXX_C_LOCALE_GNU
_GLIBCXX_LDBL_COMPAT(_ZSt16__convert_from_vIdEiPciPKcT_RKP15__locale_structi,
		     _ZSt16__convert_from_vIeEiPciPKcT_RKP15__locale_structi);
# else
_GLIBCXX_LDBL_COMPAT(_ZSt16__convert_from_vIdEiPciPKcT_RKPii,
		     _ZSt16__convert_from_vIeEiPciPKcT_RKPii);
# endif

#endif // _GLIBCXX_LONG_DOUBLE_COMPAT
