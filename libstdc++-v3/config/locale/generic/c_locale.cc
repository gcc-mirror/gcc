// Wrapper for underlying C-language localization -*- C++ -*-

// Copyright (C) 2001-2015 Free Software Foundation, Inc.
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

#include <cerrno>  // For errno
#include <cmath>  // For isinf, finite, finitef, fabs
#include <cstdlib>  // For strof, strtold
#include <cstring>
#include <cstdio>
#include <locale>
#include <limits>

#ifdef _GLIBCXX_HAVE_IEEEFP_H
#include <ieeefp.h>
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<>
    void
    __convert_to_v(const char* __s, float& __v, ios_base::iostate& __err,
		   const __c_locale&) throw()
    {
      // Assumes __s formatted for "C" locale.
      char* __old = setlocale(LC_ALL, 0);
      const size_t __len = strlen(__old) + 1;
      char* __sav = new char[__len];
      memcpy(__sav, __old, __len);
      setlocale(LC_ALL, "C");
      char* __sanity;
      bool __overflow = false;

#if !__FLT_HAS_INFINITY__
      errno = 0;
#endif

#ifdef _GLIBCXX_HAVE_STRTOF
      __v = strtof(__s, &__sanity);
#else
      double __d = strtod(__s, &__sanity);
      __v = static_cast<float>(__d);
#ifdef _GLIBCXX_HAVE_FINITEF
      if (!finitef (__v))
	__overflow = true;
#elif defined (_GLIBCXX_HAVE_FINITE)
      if (!finite (static_cast<double> (__v)))
	__overflow = true;
#elif defined (_GLIBCXX_HAVE_ISINF)
      if (isinf (static_cast<double> (__v)))
	__overflow = true;
#else
      if (fabs(__d) > numeric_limits<float>::max())
	__overflow = true;
#endif
#endif // _GLIBCXX_HAVE_STRTOF

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 23. Num_get overflow result.
      if (__sanity == __s || *__sanity != '\0')
	{
	  __v = 0.0f;
	  __err = ios_base::failbit;
	}
      else if (__overflow
#if __FLT_HAS_INFINITY__
	       || __v == numeric_limits<float>::infinity()
	       || __v == -numeric_limits<float>::infinity()
#else
	       || ((__v > 1.0f || __v < -1.0f) && errno == ERANGE)
#endif
	      )
	{
	  if (__v > 0.0f)
	    __v = numeric_limits<float>::max();
	  else
	    __v = -numeric_limits<float>::max();
	  __err = ios_base::failbit;
	}

      setlocale(LC_ALL, __sav);
      delete [] __sav;
    }

  template<>
    void
    __convert_to_v(const char* __s, double& __v, ios_base::iostate& __err,
		   const __c_locale&) throw()
    {
      // Assumes __s formatted for "C" locale.
      char* __old = setlocale(LC_ALL, 0);
      const size_t __len = strlen(__old) + 1;
      char* __sav = new char[__len];
      memcpy(__sav, __old, __len);
      setlocale(LC_ALL, "C");
      char* __sanity;

#if !__DBL_HAS_INFINITY__
      errno = 0;
#endif

      __v = strtod(__s, &__sanity);

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 23. Num_get overflow result.
      if (__sanity == __s || *__sanity != '\0')
	{
	  __v = 0.0;
	  __err = ios_base::failbit;
	}
      else if (
#if __DBL_HAS_INFINITY__
	       __v == numeric_limits<double>::infinity()
	       || __v == -numeric_limits<double>::infinity())
#else
	       (__v > 1.0 || __v < -1.0) && errno == ERANGE)
#endif
	{
	  if (__v > 0.0)
	    __v = numeric_limits<double>::max();
	  else
	    __v = -numeric_limits<double>::max();
	  __err = ios_base::failbit;
	}

      setlocale(LC_ALL, __sav);
      delete [] __sav;
    }

  template<>
    void
    __convert_to_v(const char* __s, long double& __v,
		   ios_base::iostate& __err, const __c_locale&) throw()
    {
      // Assumes __s formatted for "C" locale.
      char* __old = setlocale(LC_ALL, 0);
      const size_t __len = strlen(__old) + 1;
      char* __sav = new char[__len];
      memcpy(__sav, __old, __len);
      setlocale(LC_ALL, "C");

#if !__LDBL_HAS_INFINITY__
      errno = 0;
#endif

#if defined(_GLIBCXX_HAVE_STRTOLD) && !defined(_GLIBCXX_HAVE_BROKEN_STRTOLD)
      char* __sanity;
      __v = strtold(__s, &__sanity);

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 23. Num_get overflow result.
      if (__sanity == __s || *__sanity != '\0')
#else
      typedef char_traits<char>::int_type int_type;
      int __p = sscanf(__s, "%Lf", &__v);

      if (!__p || static_cast<int_type>(__p) == char_traits<char>::eof())
#endif
	{
	  __v = 0.0l;
	  __err = ios_base::failbit;
	}
       else if (
#if __LDBL_HAS_INFINITY__
		__v == numeric_limits<long double>::infinity()
		|| __v == -numeric_limits<long double>::infinity())
#else
		(__v > 1.0l || __v < -1.0l) && errno == ERANGE)
#endif
	{
	  if (__v > 0.0l)
	    __v = numeric_limits<long double>::max();
	  else
	    __v = -numeric_limits<long double>::max();
	  __err = ios_base::failbit;
	}

      setlocale(LC_ALL, __sav);
      delete [] __sav;
    }

  void
  locale::facet::_S_create_c_locale(__c_locale& __cloc, const char* __s,
				    __c_locale)
  {
    // Currently, the generic model only supports the "C" locale.
    // See http://gcc.gnu.org/ml/libstdc++/2003-02/msg00345.html
    __cloc = 0;
    if (strcmp(__s, "C"))
      __throw_runtime_error(__N("locale::facet::_S_create_c_locale "
			    "name not valid"));
  }

  void
  locale::facet::_S_destroy_c_locale(__c_locale& __cloc)
  { __cloc = 0; }

  __c_locale
  locale::facet::_S_clone_c_locale(__c_locale&) throw()
  { return __c_locale(); }

  __c_locale
  locale::facet::_S_lc_ctype_c_locale(__c_locale, const char*)
  { return __c_locale(); }

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
_GLIBCXX_LDBL_COMPAT(_ZSt14__convert_to_vIdEvPKcRT_RSt12_Ios_IostateRKPi, _ZSt14__convert_to_vIeEvPKcRT_RSt12_Ios_IostateRKPi);
#endif // _GLIBCXX_LONG_DOUBLE_COMPAT
