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
 * @file typelist.h
 * Contains typelist_chain definitions.
 * Typelists are an idea by Andrei Alexandrescu.
 */

#ifndef TYPELIST_HPP
#define TYPELIST_HPP

namespace __gnu_cxx
{
  // XXX namespace typelist
  // struct typelist -> struct node 

  struct null_type { };

  template<typename Root>
    struct typelist
    {
      typedef Root 	root;
    };

  // Forward declarations of functors.
  template<typename Hd, typename Typelist>
    struct chain
    {
      typedef Hd 	head;
      typedef Typelist 	tail;
    };

  template<typename Typelist0, typename Typelist1>
    struct append;

  template<typename Typelist_Typelist>
    struct typelist_append;

  template<typename Typelist, typename T>
    struct contains;
 
  template<typename Typelist, template<typename T> class Pred>
    struct filter;

  template<typename Typelist, int i>
    struct at_index;

  template<typename Fn, typename Typelist>
    struct apply;

  template<typename Typelist, template<typename T> class Transform>
    struct transform;
}


namespace __gnu_cxx
{
namespace detail
{
  // #include <ext/detail/type_utils.h>
  template<typename Type>
    struct type_to_type
    {
      typedef Type type;
    };

  template<bool Cond, typename A, typename B>
    struct cond_type;
  
  template<typename A, typename B>
    struct cond_type<true, A, B>
    {
      typedef A type;
    };
  
  template<typename A, typename B>
    struct cond_type<false, A, B>
    {
      typedef B type;
    };

  // #include <ext/detail/apply.h>
  template<typename Fn, typename Typelist_Chain>
    struct apply_;

  template<typename Fn, typename Hd, typename Tl>
    struct apply_<Fn, chain<Hd, Tl> >
    {
      void
      operator() (Fn& f)
      {
	f.operator()(type_to_type<Hd>());
	apply_<Fn, Tl> next;
	next(f);
      }
  };

  template<typename Fn>
    struct apply_<Fn, null_type>
    {
      void
      operator()(Fn&) { }
  };

  // #include <ext/detail/append.h>
  template<typename Typelist_Chain0, typename Typelist_Chain1>
    struct append_;

  template<typename Hd, typename Tl, typename Typelist_Chain>
    struct append_<chain<Hd, Tl>, Typelist_Chain>
    {
      typedef append_<Tl, Typelist_Chain> 		append_type;
      typedef chain<Hd, typename append_type::type> 	type;
    };

  template<typename Typelist_Chain>
    struct append_<null_type, Typelist_Chain>
    {
      typedef Typelist_Chain 					type;
    };

  // #include <ext/detail/contains.h>
  template<typename Typelist_Chain, typename T>
    struct contains_;

  template<typename T>
    struct contains_<null_type, T>
    {
      enum
	{
	  value = false
	};
    };

  template<typename Hd, typename Tl, typename T>
    struct contains_<chain<Hd, Tl>, T>
    {
      enum
	{
	  value = contains_<Tl, T>::value
	};
    };
  
  template<typename Tl, typename T>
    struct contains_<chain<T, Tl>, T>
    {
      enum
	{
	  value = true
	};
    };

  // #include <ext/detail/filter.h>
  template<typename Typelist_Chain, template<typename T> class Pred>
    struct chain_filter_;

  template<template<typename T> class Pred>
    struct chain_filter_<null_type, Pred>
    {
      typedef null_type type;
  };

  template<typename Hd, typename Tl, template<typename T> class Pred>
    struct chain_filter_<chain<Hd, Tl>, Pred>
    {
      enum
	{
	  include_hd = Pred<Hd>::value
	};
      
      typedef typename chain_filter_<Tl, Pred>::type 	rest_type;
      typedef chain<Hd, rest_type> 			chain_type;
      typedef typename cond_type<include_hd, chain_type, rest_type>::type type;
  };

  // #include <ext/detail/at_index.h>
  template<typename Typelist_Chain, int i>
    struct chain_at_index_;

  template<typename Hd, typename Tl>
    struct chain_at_index_<chain<Hd, Tl>, 0>
    {
      typedef Hd 						type;
    };
  
  template<typename Hd, typename Tl, int i>
    struct chain_at_index_<chain<Hd, Tl>, i>
    {
      typedef typename chain_at_index_<Tl, i - 1>::type type;
    };

  // #include <ext/detail/transform.h>
  template<class Typelist_Chain, template<typename T> class Transform>
    struct chain_transform_;

  template<template<typename T> class Transform>
    struct chain_transform_<null_type, Transform>
    {
      typedef null_type type;
    };
  
  template<class Hd, class Tl, template<typename T> class Transform>
    struct chain_transform_<chain<Hd, Tl>, Transform>
    {
      typedef typename chain_transform_<Tl, Transform>::type rest_type;
      typedef typename Transform<Hd>::type transform_type;
      typedef chain<transform_type, rest_type> type;
    };

  // #include <ext/detail/typelist_append.h>
  template<typename Typelist_Typelist_Chain>
    struct typelist_append_;

  template<typename Hd>
    struct typelist_append_<chain<Hd, null_type> >
    {
      typedef chain<Hd, null_type> type;
    };

  template<typename Hd, typename Tl>
    struct typelist_append_<chain< Hd, Tl> >
    {
    private:
      typedef typename typelist_append_<Tl>::type rest;
      
    public:
      typedef typename append<Hd, typelist<rest> >::type::root type;
    };
} // namespace detail
}


namespace __gnu_cxx
{
  template<typename Fn, typename Typelist>
    struct apply
    {
      void
      operator()(Fn& f)
      {
	typedef typename Typelist::root 			root_type;
	detail::apply_<Fn, root_type>  a;	
	a(f);
      }
    };

  template<typename Typelist0, typename Typelist1>
    struct append
    {
    private:
      typedef typename Typelist0::root 				root0_type;
      typedef typename Typelist1::root 				root1_type;
      typedef detail::append_<root0_type, root1_type> 		append_type;

    public:
      typedef typelist<typename append_type::type> 		type;
    };

  template<typename Typelist_Typelist>
    struct typelist_append
    {
    private:
      typedef typename Typelist_Typelist::root 		      	root_type;
      typedef detail::typelist_append_<root_type> 		append_type;

    public:
      typedef typelist<typename append_type::type> 		type;
    };

  template<typename Typelist, typename T>
    struct contains
    {
      typedef typename Typelist::root 				root_type;

      enum
	{
	  value = detail::contains_<root_type, T>::value
	};
    };

  template<typename Typelist, template<typename T> class Pred>
    struct filter
    {
    private:
      typedef typename Typelist::root 				root_type;
      typedef detail::chain_filter_<root_type, Pred> 		filter_type;

    public:
      typedef typelist<typename filter_type::type> 	       	type;
    };

  template<typename Typelist, int i>
    struct at_index
    {
      typedef typename Typelist::root 				root_type;
      typedef detail::chain_at_index_<root_type, i> 		index_type;
      
      typedef typename index_type::type 			type;
    };

  template<typename Typelist, template<typename T> class Transform>
    struct transform
    {
    private:
      typedef typename Typelist::root 				root_type;
      typedef detail::chain_transform_<root_type, Transform> 	transform_type;

    public:
      typedef typelist<typename transform_type::type> 		type;
    };
} // namespace __gnu_cxx


#define _GLIBCXX_TYPELIST_CHAIN1(X0) __gnu_cxx::chain<X0, __gnu_cxx::null_type>
#define _GLIBCXX_TYPELIST_CHAIN2(X0, X1) __gnu_cxx::chain<X0, _GLIBCXX_TYPELIST_CHAIN1(X1) >
#define _GLIBCXX_TYPELIST_CHAIN3(X0, X1, X2) __gnu_cxx::chain<X0, _GLIBCXX_TYPELIST_CHAIN2(X1, X2) >
#define _GLIBCXX_TYPELIST_CHAIN4(X0, X1, X2, X3) __gnu_cxx::chain<X0, _GLIBCXX_TYPELIST_CHAIN3(X1, X2, X3) >
#define _GLIBCXX_TYPELIST_CHAIN5(X0, X1, X2, X3, X4) __gnu_cxx::chain<X0, _GLIBCXX_TYPELIST_CHAIN4(X1, X2, X3, X4) >
#define _GLIBCXX_TYPELIST_CHAIN6(X0, X1, X2, X3, X4, X5) __gnu_cxx::chain<X0, _GLIBCXX_TYPELIST_CHAIN5(X1, X2, X3, X4, X5) >
#define _GLIBCXX_TYPELIST_CHAIN7(X0, X1, X2, X3, X4, X5, X6) __gnu_cxx::chain<X0, _GLIBCXX_TYPELIST_CHAIN6(X1, X2, X3, X4, X5, X6) >
#define _GLIBCXX_TYPELIST_CHAIN8(X0, X1, X2, X3, X4, X5, X6, X7) __gnu_cxx::chain<X0, _GLIBCXX_TYPELIST_CHAIN7(X1, X2, X3, X4, X5, X6, X7) >
#define _GLIBCXX_TYPELIST_CHAIN9(X0, X1, X2, X3, X4, X5, X6, X7, X8) __gnu_cxx::chain<X0, _GLIBCXX_TYPELIST_CHAIN8(X1, X2, X3, X4, X5, X6, X7, X8) >
#define _GLIBCXX_TYPELIST_CHAIN10(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9) __gnu_cxx::chain<X0, _GLIBCXX_TYPELIST_CHAIN9(X1, X2, X3, X4, X5, X6, X7, X8, X9) >
#define _GLIBCXX_TYPELIST_CHAIN11(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10) __gnu_cxx::chain<X0, _GLIBCXX_TYPELIST_CHAIN10(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10) >
#define _GLIBCXX_TYPELIST_CHAIN12(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11) __gnu_cxx::chain<X0, _GLIBCXX_TYPELIST_CHAIN11(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11) >
#define _GLIBCXX_TYPELIST_CHAIN13(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12) __gnu_cxx::chain<X0, _GLIBCXX_TYPELIST_CHAIN12(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12) >
#define _GLIBCXX_TYPELIST_CHAIN14(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13) __gnu_cxx::chain<X0, _GLIBCXX_TYPELIST_CHAIN13(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13) >
#define _GLIBCXX_TYPELIST_CHAIN15(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14) __gnu_cxx::chain<X0, _GLIBCXX_TYPELIST_CHAIN14(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14) >

#endif

