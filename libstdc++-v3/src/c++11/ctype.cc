// Copyright (C) 1997-2018 Free Software Foundation, Inc.
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

#include <locale>
#include <cstdlib>
#include <cstring>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Definitions for static const data members of ctype_base.
  const ctype_base::mask ctype_base::space;
  const ctype_base::mask ctype_base::print;
  const ctype_base::mask ctype_base::cntrl;
  const ctype_base::mask ctype_base::upper;
  const ctype_base::mask ctype_base::lower;
  const ctype_base::mask ctype_base::alpha;
  const ctype_base::mask ctype_base::digit;
  const ctype_base::mask ctype_base::punct;
  const ctype_base::mask ctype_base::xdigit;
  const ctype_base::mask ctype_base::alnum;
  const ctype_base::mask ctype_base::graph;
  const ctype_base::mask ctype_base::blank;

  // Definitions for locale::id of standard facets that are specialized.
  locale::id ctype<char>::id;

#ifdef _GLIBCXX_USE_WCHAR_T
  locale::id ctype<wchar_t>::id;
#endif

  const size_t ctype<char>::table_size;

  ctype<char>::~ctype()
  {
    _S_destroy_c_locale(_M_c_locale_ctype);
    if (_M_del)
      delete[] this->table();
  }

  // Fill in the narrowing cache and flag whether all values are
  // valid or not.  _M_narrow_ok is set to 2 if memcpy can't
  // be used.
  void
  ctype<char>::
  _M_narrow_init() const
  {
    char __tmp[sizeof(_M_narrow)];
    for (size_t __i = 0; __i < sizeof(_M_narrow); ++__i)
      __tmp[__i] = __i;
    do_narrow(__tmp, __tmp + sizeof(__tmp), 0, _M_narrow);

    _M_narrow_ok = 1;
    if (__builtin_memcmp(__tmp, _M_narrow, sizeof(_M_narrow)))
      _M_narrow_ok = 2;
    else
      {
	// Deal with the special case of zero: renarrow with a
	// different default and compare.
	char __c;
	do_narrow(__tmp, __tmp + 1, 1, &__c);
	if (__c == 1)
	  _M_narrow_ok = 2;
      }
  }

  void
  ctype<char>::
  _M_widen_init() const
  {
    char __tmp[sizeof(_M_widen)];
    for (size_t __i = 0; __i < sizeof(_M_widen); ++__i)
      __tmp[__i] = __i;
    do_widen(__tmp, __tmp + sizeof(__tmp), _M_widen);

    _M_widen_ok = 1;
    // Set _M_widen_ok to 2 if memcpy can't be used.
    if (__builtin_memcmp(__tmp, _M_widen, sizeof(_M_widen)))
      _M_widen_ok = 2;
  }

#ifdef _GLIBCXX_USE_WCHAR_T
  ctype<wchar_t>::ctype(size_t __refs)
  : __ctype_abstract_base<wchar_t>(__refs),
  _M_c_locale_ctype(_S_get_c_locale()), _M_narrow_ok(false)
  { _M_initialize_ctype(); }

  ctype<wchar_t>::ctype(__c_locale __cloc, size_t __refs)
  : __ctype_abstract_base<wchar_t>(__refs),
  _M_c_locale_ctype(_S_clone_c_locale(__cloc)), _M_narrow_ok(false)
  { _M_initialize_ctype(); }

  ctype<wchar_t>::~ctype()
  { _S_destroy_c_locale(_M_c_locale_ctype); }

  ctype_byname<wchar_t>::ctype_byname(const char* __s, size_t __refs)
  : ctype<wchar_t>(__refs)
  {
    if (std::strcmp(__s, "C") != 0 && std::strcmp(__s, "POSIX") != 0)
      {
	this->_S_destroy_c_locale(this->_M_c_locale_ctype);
	this->_S_create_c_locale(this->_M_c_locale_ctype, __s);
	this->_M_initialize_ctype();
      }
  }

  ctype_byname<wchar_t>::~ctype_byname()
  { }

#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
