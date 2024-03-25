// Explicit instantantiations for monetary facets -*- C++ -*-

// Copyright (C) 2018-2024 Free Software Foundation, Inc.
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

#ifndef C
# error "This file should not be compiled directly, only included"
#endif

#include "facet_inst_macros.h"

// This header is included multiple times, to instantiate these symbols
// for char/wchar_t and for both std::string ABIs,
// and (depending on the target) for two long double formats.

namespace std _GLIBCXX_VISIBILITY(default)
{
// use_facet and has_facet instantiations
INSTANTIATE_FACET_ACCESSORS(money_put<C>);
INSTANTIATE_FACET_ACCESSORS(money_get<C>);

_GLIBCXX_BEGIN_NAMESPACE_LDBL_OR_CXX11
  template class money_get<C, istreambuf_iterator<C> >;
  template class money_put<C, ostreambuf_iterator<C> >;

  template
    istreambuf_iterator<C>
    money_get<C, istreambuf_iterator<C> >::
    _M_extract<true>(istreambuf_iterator<C>, istreambuf_iterator<C>,
		     ios_base&, ios_base::iostate&, string&) const;

  template
    istreambuf_iterator<C>
    money_get<C, istreambuf_iterator<C> >::
    _M_extract<false>(istreambuf_iterator<C>, istreambuf_iterator<C>,
		      ios_base&, ios_base::iostate&, string&) const;

  template
    ostreambuf_iterator<C>
    money_put<C, ostreambuf_iterator<C> >::
    _M_insert<true>(ostreambuf_iterator<C>, ios_base&, C,
		    const string_type&) const;

  template
    ostreambuf_iterator<C>
    money_put<C, ostreambuf_iterator<C> >::
    _M_insert<false>(ostreambuf_iterator<C>, ios_base&, C,
		     const string_type&) const;
_GLIBCXX_END_NAMESPACE_LDBL_OR_CXX11
} // namespace std
