// localization implementation details, DragonFly version -*- C++ -*-

// Copyright (C) 2014-2017 Free Software Foundation, Inc.
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
// ISO C++ 14882: 22.8  Standard locale categories.
//

// Written by Benjamin Kosnik <bkoz@redhat.com>
// Modified for DragonFly by John Marino <gnugcc@marino.st>

#include <cstdlib>
#include <locale>
#include <stdexcept>
#include <limits>
#include <langinfo.h>
#include <xlocale.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<>
    void
    __convert_to_v(const char* __s, float& __v, ios_base::iostate& __err,
		   const __c_locale& __cloc) throw()
    {
      char* __sanity;
      __v = strtof_l(__s, &__sanity, (locale_t)__cloc);

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 23. Num_get overflow result.
      if (__sanity == __s || *__sanity != '\0')
	{
	  __v = 0.0f;
	  __err = ios_base::failbit;
	}
      else if (__v == numeric_limits<float>::infinity())
	{
	  __v = numeric_limits<float>::max();
	  __err = ios_base::failbit;
	}
      else if (__v == -numeric_limits<float>::infinity())
	{
	  __v = -numeric_limits<float>::max();
	  __err = ios_base::failbit;
	}
    }

  template<>
    void
    __convert_to_v(const char* __s, double& __v, ios_base::iostate& __err,
		   const __c_locale& __cloc) throw()
    {
      char* __sanity;
      __v = strtod_l(__s, &__sanity, (locale_t)__cloc);

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 23. Num_get overflow result.
      if (__sanity == __s || *__sanity != '\0')
	{
	  __v = 0.0;
	  __err = ios_base::failbit;
	}
      else if (__v == numeric_limits<double>::infinity())
	{
	  __v = numeric_limits<double>::max();
	  __err = ios_base::failbit;
	}
      else if (__v == -numeric_limits<double>::infinity())
	{
	  __v = -numeric_limits<double>::max();
	  __err = ios_base::failbit;
	}
    }

  template<>
    void
    __convert_to_v(const char* __s, long double& __v, ios_base::iostate& __err,
		   const __c_locale& __cloc) throw()
    {
      char* __sanity;
      __v = strtold_l(__s, &__sanity, (locale_t)__cloc);

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 23. Num_get overflow result.
      if (__sanity == __s || *__sanity != '\0')
	{
	  __v = 0.0l;
	  __err = ios_base::failbit;
	}
      else if (__v == numeric_limits<long double>::infinity())
	{
	  __v = numeric_limits<long double>::max();
	  __err = ios_base::failbit;
	}
      else if (__v == -numeric_limits<long double>::infinity())
	{
	  __v = -numeric_limits<long double>::max();
	  __err = ios_base::failbit;
	}
    }

  void
  locale::facet::_S_create_c_locale(__c_locale& __cloc, const char* __s,
				    __c_locale __old)
  {
    __cloc = (__c_locale)newlocale(LC_ALL_MASK, __s, (locale_t)__old);
    if (!__cloc)
      {
	// This named locale is not supported by the underlying OS.
	__throw_runtime_error(__N("locale::facet::_S_create_c_locale "
				  "name not valid"));
      }
  }

  void
  locale::facet::_S_destroy_c_locale(__c_locale& __cloc)
  {
    if (__cloc && _S_get_c_locale() != __cloc)
      freelocale((locale_t)__cloc);
  }

  __c_locale
  locale::facet::_S_clone_c_locale(__c_locale& __cloc) throw()
  { return (__c_locale)duplocale((locale_t)__cloc); }

  __c_locale
  locale::facet::_S_lc_ctype_c_locale(__c_locale __cloc, const char* __s)
  {
    __c_locale __dup = (__c_locale)duplocale((locale_t)__cloc);
    if (__dup == __c_locale(0))
      __throw_runtime_error(__N("locale::facet::_S_lc_ctype_c_locale "
				"duplocale error"));
    __c_locale __changed = (__c_locale)newlocale(LC_CTYPE_MASK, __s,
						 (locale_t)__dup);
    if (__changed == __c_locale(0))
      {
	freelocale((locale_t)__dup);
	__throw_runtime_error(__N("locale::facet::_S_lc_ctype_c_locale "
				  "newlocale error"));
      }
    return __changed;
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  const char* const category_names[6 + _GLIBCXX_NUM_CATEGORIES] =
    {
      "LC_CTYPE",
      "LC_NUMERIC",
      "LC_TIME",
      "LC_COLLATE",
      "LC_MONETARY",
      "LC_MESSAGES"
    };

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  const char* const* const locale::_S_categories = __gnu_cxx::category_names;

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

// XXX GLIBCXX_ABI Deprecated
#ifdef _GLIBCXX_LONG_DOUBLE_COMPAT
#define _GLIBCXX_LDBL_COMPAT(dbl, ldbl) \
  extern "C" void ldbl (void) __attribute__ ((alias (#dbl)))
_GLIBCXX_LDBL_COMPAT(_ZSt14__convert_to_vIdEvPKcRT_RSt12_Ios_IostateRKP15__locale_struct, _ZSt14__convert_to_vIeEvPKcRT_RSt12_Ios_IostateRKP15__locale_struct);
#endif // _GLIBCXX_LONG_DOUBLE_COMPAT
