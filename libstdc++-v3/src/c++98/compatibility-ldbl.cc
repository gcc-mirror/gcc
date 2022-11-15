// Compatibility symbols for -mlong-double-64 compatibility -*- C++ -*-

// Copyright (C) 2006-2022 Free Software Foundation, Inc.
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

#define _GLIBCXX_USE_CXX11_ABI 0
#include <locale>
#include <cmath>
#include <tr1/functional>

#ifdef _GLIBCXX_LONG_DOUBLE_COMPAT

#ifdef __LONG_DOUBLE_128__
#error "compatibility-ldbl.cc must be compiled with -mlong-double-64"
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
#define C char
  template class num_get<C, istreambuf_iterator<C> >;
  template class num_put<C, ostreambuf_iterator<C> >;
  template class money_get<C, istreambuf_iterator<C> >;
  template class money_put<C, ostreambuf_iterator<C> >;
  template const num_put<C>& use_facet<num_put<C> >(const locale&);
  template const num_get<C>& use_facet<num_get<C> >(const locale&);
  template const money_put<C>& use_facet<money_put<C> >(const locale&);
  template const money_get<C>& use_facet<money_get<C> >(const locale&);
  template bool has_facet<num_put<C> >(const locale&);
  template bool has_facet<num_get<C> >(const locale&);
  template bool has_facet<money_put<C> >(const locale&);
  template bool has_facet<money_get<C> >(const locale&);
  template const num_put<C>* __try_use_facet<num_put<C> >(const locale&);
  template const num_get<C>* __try_use_facet<num_get<C> >(const locale&);
  template const money_put<C>* __try_use_facet<money_put<C> >(const locale&);
  template const money_get<C>* __try_use_facet<money_get<C> >(const locale&);
#undef C
#ifdef _GLIBCXX_USE_WCHAR_T
#define C wchar_t
  template class num_get<C, istreambuf_iterator<C> >;
  template class num_put<C, ostreambuf_iterator<C> >;
  template class money_get<C, istreambuf_iterator<C> >;
  template class money_put<C, ostreambuf_iterator<C> >;
  template const num_put<C>& use_facet<num_put<C> >(const locale&);
  template const num_get<C>& use_facet<num_get<C> >(const locale&);
  template const money_put<C>& use_facet<money_put<C> >(const locale&);
  template const money_get<C>& use_facet<money_get<C> >(const locale&);
  template bool has_facet<num_put<C> >(const locale&);
  template bool has_facet<num_get<C> >(const locale&);
  template bool has_facet<money_put<C> >(const locale&);
  template bool has_facet<money_get<C> >(const locale&);
  template const num_put<C>* __try_use_facet<num_put<C> >(const locale&);
  template const num_get<C>* __try_use_facet<num_get<C> >(const locale&);
  template const money_put<C>* __try_use_facet<money_put<C> >(const locale&);
  template const money_get<C>* __try_use_facet<money_get<C> >(const locale&);
#undef C
#endif
}

// For std::tr1::hash<long double>::operator()
#define _GLIBCXX_LONG_DOUBLE_COMPAT_IMPL
#include "hash-long-double-tr1-aux.cc"

// std::tr1::hash<long double>::operator()
// and std::hash<long double>::operator()
// are the same, no need to duplicate them.
extern "C" std::size_t _ZNKSt4hashIeEclEe (long double)
  _GLIBCXX_PURE __attribute__((alias ("_ZNKSt3tr14hashIeEclEe")));

#endif
