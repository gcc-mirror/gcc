// Wrapper for underlying C-language localization -*- C++ -*-

// Copyright (C) 2001-2017 Free Software Foundation, Inc.
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

#include <locale>
#include <stdexcept>
#include <limits>
#include <algorithm>
#include <langinfo.h>
#include <bits/c++locale_internal.h>

#include <backward/auto_ptr.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<>
    void
    __convert_to_v(const char* __s, float& __v, ios_base::iostate& __err,
		   const __c_locale& __cloc) throw()
    {
      char* __sanity;
      __v = __strtof_l(__s, &__sanity, __cloc);

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
      __v = __strtod_l(__s, &__sanity, __cloc);

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
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
      // Prefer strtold_l, as __strtold_l isn't prototyped in more recent
      // glibc versions.
      __v = strtold_l(__s, &__sanity, __cloc);
#else
      __v = __strtold_l(__s, &__sanity, __cloc);
#endif

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
    __cloc = __newlocale(1 << LC_ALL, __s, __old);
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
      __freelocale(__cloc);
  }

  __c_locale
  locale::facet::_S_clone_c_locale(__c_locale& __cloc) throw()
  { return __duplocale(__cloc); }

  __c_locale
  locale::facet::_S_lc_ctype_c_locale(__c_locale __cloc, const char* __s)
  {
    __c_locale __dup = __duplocale(__cloc);
    if (__dup == __c_locale(0))
      __throw_runtime_error(__N("locale::facet::_S_lc_ctype_c_locale "
				"duplocale error"));
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
    __c_locale __changed = __newlocale(LC_CTYPE_MASK, __s, __dup);
#else
    __c_locale __changed = __newlocale(1 << LC_CTYPE, __s, __dup);
#endif
    if (__changed == __c_locale(0))
      {
	__freelocale(__dup);
	__throw_runtime_error(__N("locale::facet::_S_lc_ctype_c_locale "
				  "newlocale error"));
      }
    return __changed;
  }

  struct _CatalogIdComp
  {
    bool
    operator()(messages_base::catalog __cat, const Catalog_info* __info) const
    { return __cat < __info->_M_id; }

    bool
    operator()(const Catalog_info* __info, messages_base::catalog __cat) const
    { return __info->_M_id < __cat; }
  };

  Catalogs::~Catalogs()
  {
    for (vector<Catalog_info*>::iterator __it = _M_infos.begin();
	 __it != _M_infos.end(); ++__it)
      delete *__it;
  }

  messages_base::catalog
  Catalogs::_M_add(const char* __domain, locale __l)
  {
    __gnu_cxx::__scoped_lock lock(_M_mutex);

    // The counter is not likely to roll unless catalogs keep on being
    // opened/closed which is consider as an application mistake for the
    // moment.
    if (_M_catalog_counter == numeric_limits<messages_base::catalog>::max())
      return -1;

    auto_ptr<Catalog_info> info(new Catalog_info(_M_catalog_counter++,
						 __domain, __l));

    // Check if we managed to allocate memory for domain.
    if (!info->_M_domain)
      return -1;

    _M_infos.push_back(info.get());
    return info.release()->_M_id;
  }

  void
  Catalogs::_M_erase(messages_base::catalog __c)
  {
    __gnu_cxx::__scoped_lock lock(_M_mutex);

    vector<Catalog_info*>::iterator __res =
      lower_bound(_M_infos.begin(), _M_infos.end(), __c, _CatalogIdComp());
    if (__res == _M_infos.end() || (*__res)->_M_id != __c)
      return;

    delete *__res;
    _M_infos.erase(__res);

    // Just in case closed catalog was the last open.
    if (__c == _M_catalog_counter - 1)
      --_M_catalog_counter;
  }

  const Catalog_info*
  Catalogs::_M_get(messages_base::catalog __c) const
  {
    __gnu_cxx::__scoped_lock lock(_M_mutex);

    vector<Catalog_info*>::const_iterator __res =
      lower_bound(_M_infos.begin(), _M_infos.end(), __c, _CatalogIdComp());

    if (__res != _M_infos.end() && (*__res)->_M_id == __c)
      return *__res;

    return 0;
  }

  Catalogs&
  get_catalogs()
  {
    static Catalogs __catalogs;
    return __catalogs;
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
      "LC_MESSAGES",
      "LC_PAPER",
      "LC_NAME",
      "LC_ADDRESS",
      "LC_TELEPHONE",
      "LC_MEASUREMENT",
      "LC_IDENTIFICATION"
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
#pragma GCC diagnostic ignored "-Wattribute-alias"
#define _GLIBCXX_LDBL_COMPAT(dbl, ldbl) \
  extern "C" void ldbl (void) __attribute__ ((alias (#dbl)))
_GLIBCXX_LDBL_COMPAT(_ZSt14__convert_to_vIdEvPKcRT_RSt12_Ios_IostateRKP15__locale_struct, _ZSt14__convert_to_vIeEvPKcRT_RSt12_Ios_IostateRKP15__locale_struct);
#endif // _GLIBCXX_LONG_DOUBLE_COMPAT
