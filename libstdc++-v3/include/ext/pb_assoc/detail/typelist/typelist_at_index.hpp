// -*- C++ -*-

// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice and
// this permission notice appear in supporting documentation. None of
// the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied warranty.

/**
 * @file typelist_at_index.hpp
 * Contains typelist utilities.
 * Typelists are an idea by Andrei Alexandrescu.
 */

#ifndef TYPELIST_AT_INDEX_HPP
#define TYPELIST_AT_INDEX_HPP

#ifdef _MSC_VER
#pragma warning(disable: 4503)
#endif // #ifdef _MSC_VER

#include <ext/pb_assoc/detail/type_utils.hpp>

namespace detail
{

  template<class Typelist_Chain, int i>
  struct typelist_chain_at_index_;

  template<class Hd, class Tl>
  struct typelist_chain_at_index_<
    typelist_chain<Hd, Tl>,
    0>
  {
    typedef Hd type;
  };

  template<class Hd, class Tl, int i>
  struct typelist_chain_at_index_<
    typelist_chain<Hd, Tl>,
    i>
  {
    typedef typename typelist_chain_at_index_< Tl, i - 1>::type type;
  };

} // namespace detail

#endif // #ifndef TYPELIST_AT_INDEX_HPP

