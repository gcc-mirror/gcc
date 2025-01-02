// Copyright (C) 1997-2025 Free Software Foundation, Inc.
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

#if __cplusplus != 201103L
# error This file must be compiled as C++11
#endif

#define _GLIBCXX_USE_CXX11_ABI 1
#include <clocale>
#include <cstring>
#include <cstdlib>
#include <locale>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  using namespace __gnu_cxx;

  static inline bool
  is_C_locale(const char* s)
  {
    switch (s[0])
    {
    case 'C':
      return s[1] == '\0';
    case 'P':
      return !std::strcmp(s+1, "OSIX");
    default:
      return false;
    }
  }

  locale::locale(const char* __s) : _M_impl(0)
  {
    if (__s)
      {
	_S_initialize();
	if (is_C_locale(__s))
	  (_M_impl = _S_classic)->_M_add_reference();
	else if (*__s)
	  _M_impl = new _Impl(__s, 1);
	else
	  {
	    // Get it from the environment.
	    char* __env = std::getenv("LC_ALL");
	    // If LC_ALL is set we are done.
	    if (__env && *__env)
	      {
		if (is_C_locale(__env))
		  (_M_impl = _S_classic)->_M_add_reference();
		else
		  _M_impl = new _Impl(__env, 1);
	      }
	    else
	      {
		// LANG may set a default different from "C".
		string __lang;
		__env = std::getenv("LANG");
		if (!__env || !*__env || is_C_locale(__env))
		  __lang = "C";
		else
		  __lang = __env;

		// Scan the categories looking for the first one
		// different from LANG.
		size_t __i = 0;
		if (__lang == "C")
		  for (; __i < _S_categories_size; ++__i)
		    {
		      __env = std::getenv(_S_categories[__i]);
		      if (__env && *__env && !is_C_locale(__env))
			break;
		    }
		else
		  for (; __i < _S_categories_size; ++__i)
		    {
		      __env = std::getenv(_S_categories[__i]);
		      if (__env && *__env && __lang != __env)
			break;
		    }

		// If one is found, build the complete string of
		// the form LC_CTYPE=xxx;LC_NUMERIC=yyy; and so on...
		if (__i < _S_categories_size)
		  {
		    string __str;
		    __str.reserve(128);
		    for (size_t __j = 0; __j < __i; ++__j)
		      {
			__str += _S_categories[__j];
			__str += '=';
			__str += __lang;
			__str += ';';
		      }
		    __str += _S_categories[__i];
		    __str += '=';
		    __str += __env;
		    __str += ';';
		    ++__i;
		    for (; __i < _S_categories_size; ++__i)
		      {
			__env = std::getenv(_S_categories[__i]);
			__str += _S_categories[__i];
			if (!__env || !*__env)
			  {
			    __str += '=';
			    __str += __lang;
			    __str += ';';
			  }
			else if (is_C_locale(__env))
			  __str += "=C;";
			else
			  {
			    __str += '=';
			    __str += __env;
			    __str += ';';
			  }
		      }
		    __str.erase(__str.end() - 1);
		    _M_impl = new _Impl(__str.c_str(), 1);
		  }
		// ... otherwise either an additional instance of
		// the "C" locale or LANG.
		else if (__lang == "C")
		  (_M_impl = _S_classic)->_M_add_reference();
		else
		  _M_impl = new _Impl(__lang.c_str(), 1);
	      }
	  }
      }
    else
      __throw_runtime_error(__N("locale::locale null not valid"));
  }

  locale::locale(const locale& __base, const char* __s, category __cat)
  : _M_impl(0)
  {
    // NB: There are complicated, yet more efficient ways to do
    // this. Building up locales on a per-category way is tedious, so
    // let's do it this way until people complain.
    locale __add(__s);
    _M_coalesce(__base, __add, __cat);
  }

  locale::locale(const locale& __base, const locale& __add, category __cat)
  : _M_impl(0)
  { _M_coalesce(__base, __add, __cat); }

  void
  locale::_M_coalesce(const locale& __base, const locale& __add,
		      category __cat)
  {
    __cat = _S_normalize_category(__cat);
    _M_impl = new _Impl(*__base._M_impl, 1);

    __try
      { _M_impl->_M_replace_categories(__add._M_impl, __cat); }
    __catch(...)
      {
	_M_impl->_M_remove_reference();
	__throw_exception_again;
      }
  }

const int num_facets = (
    _GLIBCXX_NUM_FACETS + _GLIBCXX_NUM_CXX11_FACETS
#ifdef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT
    + _GLIBCXX_NUM_LBDL_ALT128_FACETS
#endif
    )
#ifdef _GLIBCXX_USE_WCHAR_T
  * 2
#endif
  + _GLIBCXX_NUM_UNICODE_FACETS;

  // Construct named _Impl.
  locale::_Impl::
  _Impl(const char* __s, size_t __refs)
  : _M_refcount(__refs), _M_facets(0), _M_facets_size(num_facets),
    _M_caches(0), _M_names(0)
  {
    // Initialize the underlying locale model, which also checks to
    // see if the given name is valid.
    __c_locale __cloc;
    locale::facet::_S_create_c_locale(__cloc, __s);
    __c_locale __clocm = __cloc;

    __try
      {
	_M_facets = new const facet*[_M_facets_size]();
	_M_caches = new const facet*[_M_facets_size]();
	_M_names = new char*[_S_categories_size]();

	// Name the categories.
	const char* __smon = __s;
	const size_t __len = std::strlen(__s);
	if (!std::memchr(__s, ';', __len))
	  {
	    _M_names[0] = new char[__len + 1];
	    std::memcpy(_M_names[0], __s, __len + 1);
	  }
	else
	  {
	    const char* __end = __s;
	    bool __found_ctype = false;
	    bool __found_monetary = false;
	    size_t __ci = 0, __mi = 0;
	    for (size_t __i = 0; __i < _S_categories_size; ++__i)
	      {
		const char* __beg = std::strchr(__end + 1, '=') + 1;
		__end = std::strchr(__beg, ';');
		if (!__end)
		  __end = __s + __len;
		_M_names[__i] = new char[__end - __beg + 1];
		std::memcpy(_M_names[__i], __beg, __end - __beg);
		_M_names[__i][__end - __beg] = '\0';
		if (!__found_ctype
		    && *(__beg - 2) == 'E' && *(__beg - 3) == 'P')
		  {
		    __found_ctype = true;
		    __ci = __i;
		  }
		else if (!__found_monetary && *(__beg - 2) == 'Y')
		  {
		    __found_monetary = true;
		    __mi = __i;
		  }
	      }

	    if (std::strcmp(_M_names[__ci], _M_names[__mi]))
	      {
		__smon = _M_names[__mi];
		__clocm = locale::facet::_S_lc_ctype_c_locale(__cloc,
							      __smon);
	      }
	  }

	// Construct all standard facets and add them to _M_facets.
	_M_init_facet(new std::ctype<char>(__cloc, 0, false));
	_M_init_facet(new codecvt<char, char, mbstate_t>(__cloc));
	_M_init_facet(new numpunct<char>(__cloc));
	_M_init_facet(new num_get<char>);
	_M_init_facet(new num_put<char>);
	_M_init_facet(new std::collate<char>(__cloc));
	_M_init_facet(new moneypunct<char, false>(__cloc, 0));
	_M_init_facet(new moneypunct<char, true>(__cloc, 0));
	_M_init_facet(new money_get<char>);
	_M_init_facet(new money_put<char>);
	_M_init_facet(new __timepunct<char>(__cloc, __s));
	_M_init_facet(new time_get<char>);
	_M_init_facet(new time_put<char>);
	_M_init_facet(new std::messages<char>(__cloc, __s));

#ifdef  _GLIBCXX_USE_WCHAR_T
	_M_init_facet(new std::ctype<wchar_t>(__cloc));
	_M_init_facet(new codecvt<wchar_t, char, mbstate_t>(__cloc));
	_M_init_facet(new numpunct<wchar_t>(__cloc));
	_M_init_facet(new num_get<wchar_t>);
	_M_init_facet(new num_put<wchar_t>);
	_M_init_facet(new std::collate<wchar_t>(__cloc));
	_M_init_facet(new moneypunct<wchar_t, false>(__clocm, __smon));
	_M_init_facet(new moneypunct<wchar_t, true>(__clocm, __smon));
	_M_init_facet(new money_get<wchar_t>);
	_M_init_facet(new money_put<wchar_t>);
	_M_init_facet(new __timepunct<wchar_t>(__cloc, __s));
	_M_init_facet(new time_get<wchar_t>);
	_M_init_facet(new time_put<wchar_t>);
	_M_init_facet(new std::messages<wchar_t>(__cloc, __s));
#endif

#if _GLIBCXX_NUM_UNICODE_FACETS != 0
        _M_init_facet(new codecvt<char16_t, char, mbstate_t>);
        _M_init_facet(new codecvt<char32_t, char, mbstate_t>);

#ifdef _GLIBCXX_USE_CHAR8_T
        _M_init_facet(new codecvt<char16_t, char8_t, mbstate_t>);
        _M_init_facet(new codecvt<char32_t, char8_t, mbstate_t>);
#endif

#endif

#if _GLIBCXX_USE_DUAL_ABI
        _M_init_extra(&__cloc, &__clocm, __s, __smon);
#endif

#ifdef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT
	_M_init_extra_ldbl128(false);
#endif

	locale::facet::_S_destroy_c_locale(__cloc);
	if (__clocm != __cloc)
	  locale::facet::_S_destroy_c_locale(__clocm);
      }
    __catch(...)
      {
	locale::facet::_S_destroy_c_locale(__cloc);
	if (__clocm != __cloc)
	  locale::facet::_S_destroy_c_locale(__clocm);
	this->~_Impl();
	__throw_exception_again;
      }
  }

  void
  locale::_Impl::
  _M_replace_categories(const _Impl* __imp, category __cat)
  {
    category __mask = 1;
    // _GLIBCXX_RESOLVE_LIB_DEFECTS
    // 3676. Name of locale composed using std::locale::none
    if (!_M_names[0] || (__cat != none && !__imp->_M_names[0]))
      {
	if (_M_names[0])
	  {
	    delete [] _M_names[0];
	    _M_names[0] = 0;   // Unnamed.
	  }

	for (size_t __ix = 0; __ix < _S_categories_size; ++__ix, __mask <<= 1)
	  {
	    if (__mask & __cat)
	      // Need to replace entry in _M_facets with other locale's info.
	      _M_replace_category(__imp, _S_facet_categories[__ix]);
	  }
      }
    else
      {
	if (!_M_names[1])
	  {
	    // A full set of _M_names must be prepared, all identical
	    // to _M_names[0] to begin with. Then, below, a few will
	    // be replaced by the corresponding __imp->_M_names. I.e.,
	    // not a "simple" locale anymore (see locale::operator==).
	    const size_t __len = std::strlen(_M_names[0]) + 1;
	    for (size_t __i = 1; __i < _S_categories_size; ++__i)
	      {
		_M_names[__i] = new char[__len];
		std::memcpy(_M_names[__i], _M_names[0], __len);
	      }
	  }

	for (size_t __ix = 0; __ix < _S_categories_size; ++__ix, __mask <<= 1)
	  {
	    if (__mask & __cat)
	      {
		// Need to replace entry in _M_facets with other locale's info.
		_M_replace_category(__imp, _S_facet_categories[__ix]);

		// FIXME: Hack for libstdc++/29217: the numerical encodings
		// of the time and collate categories are swapped vs the
		// order of the names in locale::_S_categories.  We'd like to
		// adjust the former (the latter is dictated by compatibility
		// with glibc) but we can't for binary compatibility.
		size_t __ix_name = __ix;
		if (__ix == 2 || __ix == 3)
		  __ix_name = 5 - __ix;

		char* __src = __imp->_M_names[__ix_name] ?
		              __imp->_M_names[__ix_name] : __imp->_M_names[0];
		const size_t __len = std::strlen(__src) + 1;
		char* __new = new char[__len];
		std::memcpy(__new, __src, __len);
		delete [] _M_names[__ix_name];
		_M_names[__ix_name] = __new;
	      }
	  }
      }
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
