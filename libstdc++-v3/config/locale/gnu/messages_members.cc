// std::messages implementation details, GNU version -*- C++ -*-

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
// ISO C++ 14882: 22.2.7.1.2  messages virtual functions
//

// Written by Benjamin Kosnik <bkoz@redhat.com>

#include <locale>
#include <bits/c++locale_internal.h>

#include <limits>
#include <algorithm>
#include <vector>
#include <cstdlib>	// std::free
#include <string.h>	// ::strdup

#include <backward/auto_ptr.h>
#include <ext/concurrence.h>

namespace
{
  using namespace std;

  typedef messages_base::catalog catalog;

  struct Catalog_info
    {
    Catalog_info(catalog __id, const string& __domain, locale __loc)
      : _M_id(__id), _M_domain(__domain), _M_locale(__loc)
    { }

    catalog _M_id;
    string _M_domain;
    locale _M_locale;
  };

  class Catalogs
  {
  public:
    Catalogs() : _M_catalog_counter(0) { }

    ~Catalogs()
    {
      for (vector<Catalog_info*>::iterator __it = _M_infos.begin();
	   __it != _M_infos.end(); ++__it)
	delete *__it;
    }

    catalog
    _M_add(const string& __domain, locale __l)
    {
      __gnu_cxx::__scoped_lock lock(_M_mutex);

      // The counter is not likely to roll unless catalogs keep on being
      // opened/closed which is consider as an application mistake for the
      // moment.
      if (_M_catalog_counter == numeric_limits<catalog>::max())
	return -1;

      std::auto_ptr<Catalog_info> info(new Catalog_info(_M_catalog_counter++,
							__domain, __l));
      _M_infos.push_back(info.get());
      return info.release()->_M_id;
    }

    void
    _M_erase(catalog __c)
    {
      __gnu_cxx::__scoped_lock lock(_M_mutex);

      vector<Catalog_info*>::iterator __res =
	lower_bound(_M_infos.begin(), _M_infos.end(), __c, _Comp());
      if (__res == _M_infos.end() || (*__res)->_M_id != __c)
	return;

      delete *__res;
      _M_infos.erase(__res);

      // Just in case closed catalog was the last open.
      if (__c == _M_catalog_counter - 1)
	--_M_catalog_counter;
    }

    const Catalog_info*
    _M_get(catalog __c) const
    {
      __gnu_cxx::__scoped_lock lock(_M_mutex);

      vector<Catalog_info*>::const_iterator __res =
	lower_bound(_M_infos.begin(), _M_infos.end(), __c, _Comp());

      if (__res != _M_infos.end() && (*__res)->_M_id == __c)
	return *__res;

      return 0;
    }

  private:
    struct _Comp
    {
      bool operator()(catalog __cat, const Catalog_info* __info) const
      { return __cat < __info->_M_id; }

      bool operator()(const Catalog_info* __info, catalog __cat) const
      { return __info->_M_id < __cat; }
    };

    mutable __gnu_cxx::__mutex _M_mutex;
    catalog _M_catalog_counter;
    std::vector<Catalog_info*> _M_infos;
  };

  Catalogs&
  get_catalogs()
  {
    static Catalogs __catalogs;
    return __catalogs;
  }

  const char*
  get_glibc_msg(__c_locale __locale_messages __attribute__((unused)),
		const char* __name_messages __attribute__((unused)),
		const char* __domainname,
		const char* __dfault)
  {
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
    std::__c_locale __old = __uselocale(__locale_messages);
    const char* __msg = dgettext(__domainname, __dfault);
    __uselocale(__old);
    return __msg;
#else
    if (char* __sav = strdup(setlocale(LC_ALL, 0)))
      {
	setlocale(LC_ALL, __name_messages);
	const char* __msg = dgettext(__domainname, __dfault);
	setlocale(LC_ALL, __sav);
	free(__sav);
	return __msg;
      }
    return __dfault;
#endif
  }
}

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Specializations.
  template<>
    typename messages<char>::catalog
    messages<char>::do_open(const basic_string<char>& __s,
			    const locale& __l) const
    {
      typedef codecvt<char, char, mbstate_t> __codecvt_t;
      const __codecvt_t& __codecvt = use_facet<__codecvt_t>(__l);

      bind_textdomain_codeset(__s.c_str(),
	  __nl_langinfo_l(CODESET, __codecvt._M_c_locale_codecvt));
      return get_catalogs()._M_add(__s, __l);
    }

  template<>
    void
    messages<char>::do_close(catalog __c) const
    { get_catalogs()._M_erase(__c); }

  template<>
    string
    messages<char>::do_get(catalog __c, int, int,
			   const string& __dfault) const
    {
      if (__c < 0 || __dfault.empty())
	return __dfault;

      const Catalog_info* __cat_info = get_catalogs()._M_get(__c);

      if (!__cat_info)
	return __dfault;

      return get_glibc_msg(_M_c_locale_messages, _M_name_messages,
			   __cat_info->_M_domain.c_str(),
			   __dfault.c_str());
    }

#ifdef _GLIBCXX_USE_WCHAR_T
  template<>
    typename messages<wchar_t>::catalog
    messages<wchar_t>::do_open(const basic_string<char>& __s,
			       const locale& __l) const
    {
      typedef codecvt<wchar_t, char, mbstate_t> __codecvt_t;
      const __codecvt_t& __codecvt = use_facet<__codecvt_t>(__l);

      bind_textdomain_codeset(__s.c_str(),
	  __nl_langinfo_l(CODESET, __codecvt._M_c_locale_codecvt));

      return get_catalogs()._M_add(__s, __l);
    }

  template<>
    void
    messages<wchar_t>::do_close(catalog __c) const
    { get_catalogs()._M_erase(__c); }

  template<>
    wstring
    messages<wchar_t>::do_get(catalog __c, int, int,
			      const wstring& __wdfault) const
    {
      if (__c < 0 || __wdfault.empty())
	return __wdfault;

      const Catalog_info* __cat_info = get_catalogs()._M_get(__c);

      if (!__cat_info)
	return __wdfault;

      typedef codecvt<wchar_t, char, mbstate_t> __codecvt_t;
      const __codecvt_t& __conv =
	use_facet<__codecvt_t>(__cat_info->_M_locale);

      const char* __translation;
      mbstate_t __state;
      __builtin_memset(&__state, 0, sizeof(mbstate_t));
      {
	const wchar_t* __wdfault_next;
	size_t __mb_size = __wdfault.size() * __conv.max_length();;
	char* __dfault =
	  static_cast<char*>(__builtin_alloca(sizeof(char) * (__mb_size + 1)));
	char* __dfault_next;
	__conv.out(__state,
		   __wdfault.data(), __wdfault.data() + __wdfault.size(),
		   __wdfault_next,
		   __dfault, __dfault + __mb_size, __dfault_next);

	// Make sure string passed to dgettext is \0 terminated.
	*__dfault_next = '\0';
	__translation = get_glibc_msg(_M_c_locale_messages, _M_name_messages,
				      __cat_info->_M_domain.c_str(), __dfault);

	// If we end up getting default value back we can simply return original
	// default value.
	if (__translation == __dfault)
	  return __wdfault;
      }

      __builtin_memset(&__state, 0, sizeof(mbstate_t));
      size_t __size = __builtin_strlen(__translation);
      const char* __translation_next;
      wchar_t* __wtranslation =
	static_cast<wchar_t*>(__builtin_alloca(sizeof(wchar_t) * (__size + 1)));
      wchar_t* __wtranslation_next;
      __conv.in(__state, __translation, __translation + __size,
		__translation_next,
		__wtranslation, __wtranslation + __size,
		__wtranslation_next);
      return wstring(__wtranslation, __wtranslation_next);
    }
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
