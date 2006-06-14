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
 * @file binomial_heap_base_.hpp
 * Contains an implementation class for a base of binomial heaps.
 */

#ifndef PB_DS_BINOMIAL_HEAP_BASE_HPP
#define PB_DS_BINOMIAL_HEAP_BASE_HPP

/*
 * Binomial heap base.
 * Vuillemin J is the mastah.
 * Modified from CLRS.
 */

#ifdef PB_DS_BINOMIAL_HEAP_BASE_DEBUG_
#include <cassert>
#endif // #ifdef PB_DS_BINOMIAL_HEAP_BASE_DEBUG_
#include <ext/pb_ds/detail/cond_dealtor.hpp>
#include <ext/pb_ds/detail/type_utils.hpp>
#include <ext/pb_ds/detail/left_child_next_sibling_heap_/left_child_next_sibling_heap_.hpp>
#include <ext/pb_ds/detail/left_child_next_sibling_heap_/null_metadata.hpp>

namespace pb_ds
{
  namespace detail
  {

#ifdef PB_DS_BINOMIAL_HEAP_BASE_DEBUG_
#define PB_DS_DBG_ASSERT(X) assert(X)
#define PB_DS_DBG_VERIFY(X) assert(X)
#define PB_DS_DBG_ONLY(X) X
#else // #ifdef PB_DS_BINOMIAL_HEAP_BASE_DEBUG_
#define PB_DS_DBG_ASSERT(X)
#define PB_DS_DBG_VERIFY(X) {if((X)==0);}
#define PB_DS_DBG_ONLY(X) ;
#endif // #ifdef PB_DS_BINOMIAL_HEAP_BASE_DEBUG_

#define PB_DS_CLASS_T_DEC						\
    template<typename Value_Type, class Cmp_Fn, class Allocator>

#define PB_DS_CLASS_C_DEC					\
    binomial_heap_base_<					\
						Value_Type,	\
						Cmp_Fn,		\
						Allocator>

#ifdef PB_DS_LC_NS_HEAP_DEBUG_
#define PB_DS_BASE_C_DEC						\
    left_child_next_sibling_heap_<			\
									Value_Type, \
									Cmp_Fn,	\
									typename Allocator::size_type, \
									Allocator, \
									false>
#else // #ifdef PB_DS_LC_NS_HEAP_DEBUG_
#define PB_DS_BASE_C_DEC						\
    left_child_next_sibling_heap_<			\
									Value_Type, \
									Cmp_Fn,	\
									typename Allocator::size_type, \
									Allocator>
#endif // #ifdef PB_DS_LC_NS_HEAP_DEBUG_

    /**
     * class description = "8y|\|0|\/|i41 h34p 74813">
     **/
    template<typename Value_Type, class Cmp_Fn, class Allocator>
    class binomial_heap_base_ : public PB_DS_BASE_C_DEC
    {

    private:
      typedef PB_DS_BASE_C_DEC base_type;

    protected:
      typedef typename base_type::node node;

      typedef typename base_type::node_pointer node_pointer;

      typedef typename base_type::const_node_pointer const_node_pointer;

    public:

      typedef typename Allocator::size_type size_type;

      typedef typename Allocator::difference_type difference_type;

      typedef Value_Type value_type;

      typedef
      typename Allocator::template rebind<
	value_type>::other::pointer
      pointer;

      typedef
      typename Allocator::template rebind<
	value_type>::other::const_pointer
      const_pointer;

      typedef
      typename Allocator::template rebind<
	value_type>::other::reference
      reference;

      typedef
      typename Allocator::template rebind<
	value_type>::other::const_reference
      const_reference;

      typedef
      typename PB_DS_BASE_C_DEC::const_point_iterator
      const_point_iterator;

      typedef typename PB_DS_BASE_C_DEC::point_iterator point_iterator;

      typedef typename PB_DS_BASE_C_DEC::const_iterator const_iterator;

      typedef typename PB_DS_BASE_C_DEC::iterator iterator;

      typedef Cmp_Fn cmp_fn;

      typedef Allocator allocator;

    public:

      inline point_iterator
      push(const_reference r_val);

      void
      modify(point_iterator it, const_reference r_new_val);

      inline const_reference
      top() const;

      void
      pop();

      void
      erase(point_iterator it);

      inline void
      clear();

      template<typename Pred>
      size_type
      erase_if(Pred pred);

      template<typename Pred>
      void
      split(Pred pred, PB_DS_CLASS_C_DEC& other);

      void
      join(PB_DS_CLASS_C_DEC& other);

    protected:

      binomial_heap_base_();

      binomial_heap_base_(const Cmp_Fn& r_cmp_fn);

      binomial_heap_base_(const PB_DS_CLASS_C_DEC& other);

      void
      swap(PB_DS_CLASS_C_DEC& other);

      ~binomial_heap_base_();

      template<typename It>
      void
      copy_from_range(It first_it, It last_it);

      inline void
      find_max();

#ifdef PB_DS_BINOMIAL_HEAP_BASE_DEBUG_

      void
      assert_valid(bool strictly_binomial) const;

      void
      assert_max() const;

#endif // #ifdef PB_DS_BINOMIAL_HEAP_BASE_DEBUG_

    private:

      inline node_pointer
      fix(node_pointer p_nd) const;

      inline void
      insert_node(node_pointer p_nd);

      inline void
      remove_parentless_node(node_pointer p_nd);

      inline node_pointer
      join(node_pointer p_lhs, node_pointer p_rhs) const;

#ifdef PB_DS_BINOMIAL_HEAP_BASE_DEBUG_

      void
      assert_node_consistent(const_node_pointer p_nd, bool strictly_binomial, bool increasing) const;

#endif // #ifdef PB_DS_BINOMIAL_HEAP_BASE_DEBUG_

    protected:
      node_pointer m_p_max;
    };

#include <ext/pb_ds/detail/binomial_heap_base_/constructors_destructor_fn_imps.hpp>
#include <ext/pb_ds/detail/binomial_heap_base_/debug_fn_imps.hpp>
#include <ext/pb_ds/detail/binomial_heap_base_/find_fn_imps.hpp>
#include <ext/pb_ds/detail/binomial_heap_base_/insert_fn_imps.hpp>
#include <ext/pb_ds/detail/binomial_heap_base_/erase_fn_imps.hpp>
#include <ext/pb_ds/detail/binomial_heap_base_/split_join_fn_imps.hpp>

#undef PB_DS_CLASS_C_DEC

#undef PB_DS_CLASS_T_DEC

#undef PB_DS_BASE_C_DEC

#undef PB_DS_DBG_ASSERT
#undef PB_DS_DBG_VERIFY
#undef PB_DS_DBG_ONLY

  } // namespace detail
} // namespace pb_ds

#endif // #ifndef PB_DS_BINOMIAL_HEAP_BASE_HPP
