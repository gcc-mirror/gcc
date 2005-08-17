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
 * @file typelist.hpp
 * Contains typelist_chain definitions.
 * Typelists are an idea by Andrei Alexandrescu.
 */

#ifndef TYPELIST_HPP
#define TYPELIST_HPP

#ifdef _MSC_VER
#pragma warning(disable: 4503)
#endif // #ifdef _MSC_VER

#include <ext/pb_assoc/detail/type_utils.hpp>

namespace pb_assoc
{

  namespace detail
  {

    struct null_type
    { };

    template<typename Hd, typename Tl>
    struct typelist_chain
    {
      typedef Hd head;

      typedef Tl tail;
    };

    template<class Root>
    struct typelist
    {
      typedef Root root;
    };

#define PB_ASSOC_TYPELIST_CHAIN1(X0) pb_assoc::detail::typelist_chain<X0, pb_assoc::detail::null_type>
#define PB_ASSOC_TYPELIST_CHAIN2(X0, X1) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN1(X1) >
#define PB_ASSOC_TYPELIST_CHAIN3(X0, X1, X2) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN2(X1, X2) >
#define PB_ASSOC_TYPELIST_CHAIN4(X0, X1, X2, X3) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN3(X1, X2, X3) >
#define PB_ASSOC_TYPELIST_CHAIN5(X0, X1, X2, X3, X4) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN4(X1, X2, X3, X4) >
#define PB_ASSOC_TYPELIST_CHAIN6(X0, X1, X2, X3, X4, X5) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN5(X1, X2, X3, X4, X5) >
#define PB_ASSOC_TYPELIST_CHAIN7(X0, X1, X2, X3, X4, X5, X6) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN6(X1, X2, X3, X4, X5, X6) >
#define PB_ASSOC_TYPELIST_CHAIN8(X0, X1, X2, X3, X4, X5, X6, X7) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN7(X1, X2, X3, X4, X5, X6, X7) >
#define PB_ASSOC_TYPELIST_CHAIN9(X0, X1, X2, X3, X4, X5, X6, X7, X8) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN8(X1, X2, X3, X4, X5, X6, X7, X8) >
#define PB_ASSOC_TYPELIST_CHAIN10(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN9(X1, X2, X3, X4, X5, X6, X7, X8, X9) >
#define PB_ASSOC_TYPELIST_CHAIN11(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN10(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10) >
#define PB_ASSOC_TYPELIST_CHAIN12(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN11(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11) >
#define PB_ASSOC_TYPELIST_CHAIN13(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN12(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12) >
#define PB_ASSOC_TYPELIST_CHAIN14(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN13(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13) >
#define PB_ASSOC_TYPELIST_CHAIN15(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN14(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14) >

#include <ext/pb_assoc/detail/typelist/typelist_apply.hpp>

    template<class Fn, class Typelist>
    void
    typelist_apply(Fn& r_fn, Typelist /*tl*/)
    {
      detail::apply_<
	Fn,
	typename Typelist::root>
	a;

      a(r_fn);
    }

#include <ext/pb_assoc/detail/typelist/typelist_append.hpp>

    template<class Typelist0, class Typelist1>
    struct typelist_append
    {
    private:
      typedef
      typename detail::typelist_append_<
	typename Typelist0::root,
	typename Typelist1::root>::type
      res_hd;

    public:
      typedef typelist< res_hd> type;
    };

#include <ext/pb_assoc/detail/typelist/typelist_typelist_append.hpp>

    template<class Typelist_Typelist>
    struct typelist_typelist_append
    {
    private:
      typedef
      typename detail::typelist_typelist_append_<
	typename Typelist_Typelist::root>::type
      res_hd;

    public:
      typedef typelist< res_hd> type;
    };

#include <ext/pb_assoc/detail/typelist/typelist_contains.hpp>

    template<class Typelist, class T>
    struct typelist_contains
    {
      enum
	{
	  value =
	  detail::typelist_contains_<
	  typename Typelist::root,
	  T>::value
	};
    };

#include <ext/pb_assoc/detail/typelist/typelist_filter.hpp>

    template<class Typelist, template<typename T>
    class Pred>
    struct typelist_filter
    {
    private:
      typedef
      typename detail::typelist_chain_filter_<
	typename Typelist::root,
	Pred>::type
      root_type;

    public:
      typedef typelist< root_type> type;
    };

#include <ext/pb_assoc/detail/typelist/typelist_at_index.hpp>

    template<class Typelist, int i>
    struct typelist_at_index
    {
      typedef
      typename detail::typelist_chain_at_index_<
	typename Typelist::root,
	i>::type
      type;
    };

#include <ext/pb_assoc/detail/typelist/typelist_transform.hpp>

    template<class Typelist, template<class T>
    class Transform>
    struct typelist_transform
    {
    private:
      typedef
      typename detail::typelist_chain_transform_<
	typename Typelist::root,
	Transform>::type
      root_type;

    public:
      typedef typelist< root_type> type;
    };

#undef PB_ASSOC_TYPELIST_CHAIN1
#undef PB_ASSOC_TYPELIST_CHAIN2
#undef PB_ASSOC_TYPELIST_CHAIN3
#undef PB_ASSOC_TYPELIST_CHAIN4
#undef PB_ASSOC_TYPELIST_CHAIN5
#undef PB_ASSOC_TYPELIST_CHAIN6
#undef PB_ASSOC_TYPELIST_CHAIN7
#undef PB_ASSOC_TYPELIST_CHAIN8
#undef PB_ASSOC_TYPELIST_CHAIN9
#undef PB_ASSOC_TYPELIST_CHAIN10
#undef PB_ASSOC_TYPELIST_CHAIN11
#undef PB_ASSOC_TYPELIST_CHAIN12
#undef PB_ASSOC_TYPELIST_CHAIN13
#undef PB_ASSOC_TYPELIST_CHAIN14
#undef PB_ASSOC_TYPELIST_CHAIN15

  } // namespace detail

} // namespace pb_assoc

#endif // #ifndef TYPELIST_HPP

