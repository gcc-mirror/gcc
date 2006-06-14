// -*- C++ -*-

// Copyright (C) 2005, 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice
// and this permission notice appear in supporting documentation. None
// of the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied
// warranty.

/**
 * @file typelist.hpp
 * Contains typelist_chain definitions.
 * Typelists are an idea by Andrei Alexandrescu.
 */

#ifndef PB_DS_TYPELIST_HPP
#define PB_DS_TYPELIST_HPP

#include <ext/pb_ds/detail/type_utils.hpp>

namespace pb_ds
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

    template<typename Root>
    struct typelist
    {
      typedef Root root;
    };

#define PB_DS_TYPELIST_CHAIN1(X0) typelist_chain<X0, null_type>
#define PB_DS_TYPELIST_CHAIN2(X0, X1) typelist_chain<X0, PB_DS_TYPELIST_CHAIN1(X1) >
#define PB_DS_TYPELIST_CHAIN3(X0, X1, X2) typelist_chain<X0, PB_DS_TYPELIST_CHAIN2(X1, X2) >
#define PB_DS_TYPELIST_CHAIN4(X0, X1, X2, X3) typelist_chain<X0, PB_DS_TYPELIST_CHAIN3(X1, X2, X3) >
#define PB_DS_TYPELIST_CHAIN5(X0, X1, X2, X3, X4) typelist_chain<X0, PB_DS_TYPELIST_CHAIN4(X1, X2, X3, X4) >
#define PB_DS_TYPELIST_CHAIN6(X0, X1, X2, X3, X4, X5) typelist_chain<X0, PB_DS_TYPELIST_CHAIN5(X1, X2, X3, X4, X5) >
#define PB_DS_TYPELIST_CHAIN7(X0, X1, X2, X3, X4, X5, X6) typelist_chain<X0, PB_DS_TYPELIST_CHAIN6(X1, X2, X3, X4, X5, X6) >
#define PB_DS_TYPELIST_CHAIN8(X0, X1, X2, X3, X4, X5, X6, X7) typelist_chain<X0, PB_DS_TYPELIST_CHAIN7(X1, X2, X3, X4, X5, X6, X7) >
#define PB_DS_TYPELIST_CHAIN9(X0, X1, X2, X3, X4, X5, X6, X7, X8) typelist_chain<X0, PB_DS_TYPELIST_CHAIN8(X1, X2, X3, X4, X5, X6, X7, X8) >
#define PB_DS_TYPELIST_CHAIN10(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9) typelist_chain<X0, PB_DS_TYPELIST_CHAIN9(X1, X2, X3, X4, X5, X6, X7, X8, X9) >
#define PB_DS_TYPELIST_CHAIN11(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10) typelist_chain<X0, PB_DS_TYPELIST_CHAIN10(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10) >
#define PB_DS_TYPELIST_CHAIN12(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11) typelist_chain<X0, PB_DS_TYPELIST_CHAIN11(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11) >
#define PB_DS_TYPELIST_CHAIN13(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12) typelist_chain<X0, PB_DS_TYPELIST_CHAIN12(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12) >
#define PB_DS_TYPELIST_CHAIN14(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13) typelist_chain<X0, PB_DS_TYPELIST_CHAIN13(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13) >
#define PB_DS_TYPELIST_CHAIN15(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14) typelist_chain<X0, PB_DS_TYPELIST_CHAIN14(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14) >

#include <ext/pb_ds/detail/typelist/typelist_apply.hpp>

    template<typename Fn, class Typelist>
    void
    typelist_apply(Fn& r_fn, Typelist)
    {
      detail::apply_<Fn, typename Typelist::root> a;
      a(r_fn);
    }

#include <ext/pb_ds/detail/typelist/typelist_append.hpp>

    template<typename Typelist0, class Typelist1>
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

#include <ext/pb_ds/detail/typelist/typelist_typelist_append.hpp>

    template<typename Typelist_Typelist>
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

#include <ext/pb_ds/detail/typelist/typelist_contains.hpp>

    template<typename Typelist, typename T>
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

#include <ext/pb_ds/detail/typelist/typelist_filter.hpp>

    template<typename Typelist, template<typename T>
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

#include <ext/pb_ds/detail/typelist/typelist_at_index.hpp>

    template<typename Typelist, int i>
    struct typelist_at_index
    {
      typedef
      typename detail::typelist_chain_at_index_<
	typename Typelist::root,
	i>::type
      type;
    };

#include <ext/pb_ds/detail/typelist/typelist_transform.hpp>

    template<typename Typelist, template<typename T>
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

#include <ext/pb_ds/detail/typelist/typelist_flatten.hpp>

    template<typename Typelist_Typelist>
    struct typelist_flatten
    {
    private:
      typedef
      typename detail::typelist_chain_flatten_<
      typename Typelist_Typelist::root>::type
      root_type;

    public:
      typedef typelist< root_type> type;
    };

    template<typename Typelist>
    struct typelist_from_first
    {
    private:
      typedef typename typelist_at_index< Typelist, 0>::type first_type;

    public:
      typedef typelist< typelist_chain< first_type, null_type> > type;
    };

    template<typename T0>
    struct typelist1
    {
      typedef typelist< PB_DS_TYPELIST_CHAIN1( T0)> type;
    };

    template<typename T0, typename T1>
    struct typelist2
    {
      typedef
      typelist<
	PB_DS_TYPELIST_CHAIN2(                T0,  T1)>
      type;
    };

    template<typename T0, typename T1, typename T2>
    struct typelist3
    {
      typedef
      typelist<
	PB_DS_TYPELIST_CHAIN3(                T0,  T1,  T2)>
      type;
    };

    template<typename T0, typename T1, typename T2, typename T3>
    struct typelist4
    {
      typedef
      typelist<
	PB_DS_TYPELIST_CHAIN4(                T0,  T1,  T2,  T3)>
      type;
    };

    template<typename T0,
	     typename T1,
	     typename T2,
	     typename T3,
	     typename T4>
    struct typelist5
    {
      typedef
      typelist<
	PB_DS_TYPELIST_CHAIN5(                T0,  T1,  T2,  T3,  T4)>
      type;
    };

    template<typename T0,
	     typename T1,
	     typename T2,
	     typename T3,
	     typename T4,
	     typename T5>
    struct typelist6
    {
      typedef
      typelist<
	PB_DS_TYPELIST_CHAIN6(                T0,  T1,  T2,  T3,  T4,  T5)>
      type;
    };

#undef PB_DS_TYPELIST_CHAIN1
#undef PB_DS_TYPELIST_CHAIN2
#undef PB_DS_TYPELIST_CHAIN3
#undef PB_DS_TYPELIST_CHAIN4
#undef PB_DS_TYPELIST_CHAIN5
#undef PB_DS_TYPELIST_CHAIN6
#undef PB_DS_TYPELIST_CHAIN7
#undef PB_DS_TYPELIST_CHAIN8
#undef PB_DS_TYPELIST_CHAIN9
#undef PB_DS_TYPELIST_CHAIN10
#undef PB_DS_TYPELIST_CHAIN11
#undef PB_DS_TYPELIST_CHAIN12
#undef PB_DS_TYPELIST_CHAIN13
#undef PB_DS_TYPELIST_CHAIN14
#undef PB_DS_TYPELIST_CHAIN15

  } // namespace detail

} // namespace pb_ds

#endif // #ifndef PB_DS_TYPELIST_HPP

