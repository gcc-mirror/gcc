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
 * @file head.hpp
 * Contains a leaf for a patricia tree.
 */

#ifndef PB_DS_PAT_TRIE_IHEAD_HPP
#define PB_DS_PAT_TRIE_IHEAD_HPP

#ifdef PB_DS_PAT_TRIE_DEBUG_
#include <cassert>
#endif // #ifdef PB_DS_PAT_TRIE_DEBUG_
#include <ext/pb_ds/detail/pat_trie_/node_base.hpp>

namespace pb_ds
{
  namespace detail
  {

#define PB_DS_CLASS_T_DEC						\
    template<								\
						class Type_Traits,	\
						class E_Access_Traits,	\
						class Metadata,		\
						class Allocator>

#define PB_DS_CLASS_C_DEC						\
    pat_trie_head<							\
						Type_Traits,		\
						E_Access_Traits,	\
						Metadata,		\
						Allocator>

#define PB_DS_BASE_C_DEC					\
    pat_trie_node_base<						\
					Type_Traits,		\
					E_Access_Traits,	\
					Metadata,		\
					Allocator>

#ifdef PB_DS_PAT_TRIE_DEBUG_
#define PB_DS_DBG_ASSERT(X) assert(X)
#define PB_DS_DBG_VERIFY(X) assert(X)
#define PB_DS_DBG_ONLY(X) X
#else // #ifdef PB_DS_PAT_TRIE_DEBUG_
#define PB_DS_DBG_ASSERT(X)
#define PB_DS_DBG_VERIFY(X) {if((X)==0);}
#define PB_DS_DBG_ONLY(X) ;
#endif // #ifdef PB_DS_PAT_TRIE_DEBUG_

    template<typename Type_Traits,
	     class E_Access_Traits,
	     class Metadata,
	     class Allocator>
    struct pat_trie_head : public PB_DS_BASE_C_DEC
    {
    private:
      typedef E_Access_Traits e_access_traits;

      typedef
      typename Allocator::template rebind<
	e_access_traits>::other::const_pointer
      const_e_access_traits_pointer;

      typedef
      typename Allocator::template rebind<
	PB_DS_BASE_C_DEC>::other::pointer
      node_pointer;

#ifdef PB_DS_PAT_TRIE_DEBUG_
      typedef
      typename PB_DS_BASE_C_DEC::subtree_debug_info
      subtree_debug_info;
#endif // #ifdef PB_DS_PAT_TRIE_DEBUG_

    public:
      pat_trie_head();

#ifdef PB_DS_PAT_TRIE_DEBUG_
      virtual subtree_debug_info
      assert_valid_imp(const_e_access_traits_pointer p_traits) const;
#endif // #ifdef PB_DS_PAT_TRIE_DEBUG_

    public:
      node_pointer m_p_min;

      node_pointer m_p_max;
    };

    PB_DS_CLASS_T_DEC
    PB_DS_CLASS_C_DEC::
    pat_trie_head() :
      PB_DS_BASE_C_DEC(pat_trie_head_node_type)
    { }

#ifdef PB_DS_PAT_TRIE_DEBUG_
    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::subtree_debug_info
    PB_DS_CLASS_C_DEC::
    assert_valid_imp(const_e_access_traits_pointer /*p_traits*/) const
    {
      PB_DS_DBG_ASSERT(false);

      return (subtree_debug_info());
    }
#endif // #ifdef PB_DS_PAT_TRIE_DEBUG_

#undef PB_DS_CLASS_T_DEC

#undef PB_DS_CLASS_C_DEC

#undef PB_DS_BASE_C_DEC

#undef PB_DS_DBG_ASSERT
#undef PB_DS_DBG_VERIFY
#undef PB_DS_DBG_ONLY

  } // namespace detail
} // namespace pb_ds

#endif // #ifndef PB_DS_PAT_TRIE_HEAD_HPP

