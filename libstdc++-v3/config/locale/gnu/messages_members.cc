// std::messages implementation details, GNU version -*- C++ -*-

// Copyright (C) 2001-2014 Free Software Foundation, Inc.
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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

//
// ISO C++ 14882: 22.2.7.1.2  messages virtual functions
//

// Written by Benjamin Kosnik <bkoz@redhat.com>

#include <locale>
#include <bits/c++locale_internal.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Specializations.
  template<>
    string
    messages<char>::do_get(catalog, int, int, const string& __dfault) const
    {
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
      __c_locale __old = __uselocale(_M_c_locale_messages);
      const char* __msg = const_cast<const char*>(gettext(__dfault.c_str()));
      __uselocale(__old);
      return string(__msg);
#else
      char* __old = setlocale(LC_ALL, 0);
      const size_t __len = strlen(__old) + 1;
      char* __sav = new char[__len];
      memcpy(__sav, __old, __len);
      setlocale(LC_ALL, _M_name_messages);
      const char* __msg = gettext(__dfault.c_str());
      setlocale(LC_ALL, __sav);
      delete [] __sav;
      return string(__msg);
#endif
    }

#ifdef _GLIBCXX_USE_WCHAR_T
  template<>
    wstring
    messages<wchar_t>::do_get(catalog, int, int, const wstring& __dfault) const
    {
# if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
      __c_locale __old = __uselocale(_M_c_locale_messages);
      char* __msg = gettext(_M_convert_to_char(__dfault));
      __uselocale(__old);
      return _M_convert_from_char(__msg);
# else
      char* __old = setlocale(LC_ALL, 0);
      const size_t __len = strlen(__old) + 1;
      char* __sav = new char[__len];
      memcpy(__sav, __old, __len);
      setlocale(LC_ALL, _M_name_messages);
      char* __msg = gettext(_M_convert_to_char(__dfault));
      setlocale(LC_ALL, __sav);
      delete [] __sav;
      return _M_convert_from_char(__msg);
# endif
    }
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
