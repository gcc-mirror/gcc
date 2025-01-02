// Locale support -*- C++ -*-

// Copyright (C) 1999-2025 Free Software Foundation, Inc.
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
// ISO C++ 14882: 22.1  Locales
//

#ifndef _GLIBCXX_USE_CXX11_ABI
// Instantiations in this file use the old COW std::string ABI unless included
// by another file which defines _GLIBCXX_USE_CXX11_ABI=1. Some instantiations
// are guarded by a check for !_GLIBCXX_USE_CXX11_ABI so that they are only
// instantiated once, because they are not tagged with abi_tag so should not
// be instantiated twice.
# define _GLIBCXX_USE_CXX11_ABI 0
#endif

#include <locale>

// Instantiation configuration.
#ifndef C
# define C char
# define C_is_char
#endif

#include "locale-inst-numeric.h"
#include "locale-inst-monetary.h"
#include "facet_inst_macros.h"

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // moneypunct, money_get, and money_put
#if ! _GLIBCXX_USE_CXX11_ABI
  template struct __moneypunct_cache<C, false>;
  template struct __moneypunct_cache<C, true>;
#endif
_GLIBCXX_BEGIN_NAMESPACE_CXX11
  template class moneypunct<C, false>;
  template class moneypunct<C, true>;
  template class moneypunct_byname<C, false>;
  template class moneypunct_byname<C, true>;
_GLIBCXX_END_NAMESPACE_CXX11

  // numpunct, numpunct_byname, num_get, and num_put
#if ! _GLIBCXX_USE_CXX11_ABI
  template struct __numpunct_cache<C>;
#endif
_GLIBCXX_BEGIN_NAMESPACE_CXX11
  template class numpunct<C>;
  template class numpunct_byname<C>;
_GLIBCXX_END_NAMESPACE_CXX11

  // time_get and time_put
#if ! _GLIBCXX_USE_CXX11_ABI
  template class __timepunct<C>;
  template struct __timepunct_cache<C>;
  template class time_put<C, ostreambuf_iterator<C> >;
  template class time_put_byname<C, ostreambuf_iterator<C> >;
#else
  // Instantiate constructor taking __cxx11::string
  template time_put_byname<C>::time_put_byname(const string&, size_t);
#endif
_GLIBCXX_BEGIN_NAMESPACE_CXX11
  template class time_get<C, istreambuf_iterator<C> >;
  template class time_get_byname<C, istreambuf_iterator<C> >;
_GLIBCXX_END_NAMESPACE_CXX11

  // messages
_GLIBCXX_BEGIN_NAMESPACE_CXX11
  template class messages<C>;
  template class messages_byname<C>;
_GLIBCXX_END_NAMESPACE_CXX11

  // ctype
  ctype_byname<C>::ctype_byname(const string& __s, size_t __refs)
  : ctype_byname(__s.c_str(), __refs) { }

#if ! _GLIBCXX_USE_CXX11_ABI
  inline template class __ctype_abstract_base<C>;
  template class ctype_byname<C>;
#endif

  // codecvt
#if ! _GLIBCXX_USE_CXX11_ABI
  inline template class __codecvt_abstract_base<C, char, mbstate_t>;
  template class codecvt_byname<C, char, mbstate_t>;
#else
  // Instantiate constructor taking __cxx11::string
  template codecvt_byname<C, char, mbstate_t>::codecvt_byname(const string&, size_t);
#endif

  // collate
_GLIBCXX_BEGIN_NAMESPACE_CXX11
  template class collate<C>;
  template class collate_byname<C>;
_GLIBCXX_END_NAMESPACE_CXX11

// use_facet and has_facet instantiations
#if ! _GLIBCXX_USE_CXX11_ABI
INSTANTIATE_FACET_ACCESSORS(ctype<C>);
INSTANTIATE_FACET_ACCESSORS(codecvt<C, char, mbstate_t>);
#endif
INSTANTIATE_FACET_ACCESSORS(collate<C>);
INSTANTIATE_FACET_ACCESSORS(numpunct<C>);
INSTANTIATE_FACET_ACCESSORS(moneypunct<C, false>);
// No explicit instantiation of has_facet<moneypunct<C, true>> for some reason.
INSTANTIATE_USE_FACET      (moneypunct<C, true>);
#if ! _GLIBCXX_USE_CXX11_ABI
INSTANTIATE_FACET_ACCESSORS(__timepunct<C>);
INSTANTIATE_FACET_ACCESSORS(time_put<C>);
#endif
INSTANTIATE_FACET_ACCESSORS(time_get<C>);
INSTANTIATE_FACET_ACCESSORS(messages<C>);

#if ! _GLIBCXX_USE_CXX11_ABI
  // locale functions.
  template
    C*
    __add_grouping<C>(C*, C, char const*, size_t,
			 C const*, C const*);

  template class __pad<C, char_traits<C> >;

  template
    int
    __int_to_char(C*, unsigned long, const C*,
		  ios_base::fmtflags, bool);

#ifdef _GLIBCXX_USE_LONG_LONG
  template
    int
    __int_to_char(C*, unsigned long long, const C*,
		  ios_base::fmtflags, bool);
#endif
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

// XXX GLIBCXX_ABI Deprecated
#if defined _GLIBCXX_LONG_DOUBLE_COMPAT && ! _GLIBCXX_USE_CXX11_ABI
#include "compatibility-ldbl-facets-aliases.h"
#endif // _GLIBCXX_LONG_DOUBLE_COMPAT
