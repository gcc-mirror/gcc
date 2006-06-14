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
 * @file internal_node.hpp
 * Contains an internal PB_DS_BASE_C_DEC for a patricia tree.
 */

#ifndef PB_DS_PAT_TRIE_INTERNAL_NODE_HPP
#define PB_DS_PAT_TRIE_INTERNAL_NODE_HPP

#ifdef PB_DS_PAT_TRIE_DEBUG_
#include <cassert>
#endif // #ifdef PB_DS_PAT_TRIE_DEBUG_

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
    pat_trie_internal_node<						\
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

#define PB_DS_LEAF_C_DEC						\
    pat_trie_leaf<							\
						Type_Traits,		\
						E_Access_Traits,	\
						Metadata,		\
						Allocator>

#define PB_DS_STATIC_ASSERT(UNIQUE, E)					\
    typedef								\
    static_assert_dumclass<				\
									sizeof(static_assert<(bool)(E)>)> \
    UNIQUE##static_assert_type

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
    struct pat_trie_internal_node : public PB_DS_BASE_C_DEC
    {
    public:
      enum
	{
	  arr_size =
	  E_Access_Traits::max_size + 1
	};

    private:
      typedef typename Allocator::size_type size_type;

      typedef E_Access_Traits e_access_traits;

      typedef
      typename Allocator::template rebind<
	e_access_traits>::other::const_pointer
      const_e_access_traits_pointer;

      typedef typename e_access_traits::const_iterator const_e_iterator;

      typedef
      typename Allocator::template rebind<
	PB_DS_BASE_C_DEC >::other::pointer
      node_pointer;

      typedef
      typename Allocator::template rebind<
	PB_DS_BASE_C_DEC >::other::const_pointer
      const_node_pointer;

      typedef PB_DS_LEAF_C_DEC leaf;

      typedef
      typename Allocator::template rebind<
	leaf>::other
      leaf_allocator;

      typedef typename leaf_allocator::pointer leaf_pointer;

      typedef typename leaf_allocator::const_pointer const_leaf_pointer;

      typedef
      typename Allocator::template rebind<
	PB_DS_CLASS_C_DEC>::other
      internal_node_allocator;

      typedef
      typename Allocator::template rebind<
	PB_DS_CLASS_C_DEC>::other::const_pointer
      const_internal_node_pointer;

      typedef
      typename Allocator::template rebind<
	PB_DS_CLASS_C_DEC>::other::pointer
      internal_node_pointer;

      PB_DS_STATIC_ASSERT(min_arr_size, arr_size >= 2);

      typedef typename Type_Traits::value_type value_type;

#ifdef PB_DS_PAT_TRIE_DEBUG_
      typedef
      typename PB_DS_BASE_C_DEC::subtree_debug_info
      subtree_debug_info;
#endif // #ifdef PB_DS_PAT_TRIE_DEBUG_

      typedef PB_DS_BASE_C_DEC base_type;

    private:
      inline size_type
      get_pref_pos(const_e_iterator b_it, const_e_iterator e_it, const_e_access_traits_pointer p_traits) const;

#ifdef PB_DS_PAT_TRIE_DEBUG_
      virtual subtree_debug_info
      assert_valid_imp(const_e_access_traits_pointer p_traits) const;
#endif // #ifdef PB_DS_PAT_TRIE_DEBUG_

    public:
      typedef
      typename Allocator::template rebind<
      node_pointer>::other::pointer
      node_pointer_pointer;

      typedef
      typename Allocator::template rebind<
	node_pointer>::other::reference
      node_pointer_reference;

#include <ext/pb_ds/detail/pat_trie_/const_child_iterator.hpp>
#include <ext/pb_ds/detail/pat_trie_/child_iterator.hpp>

    public:
      pat_trie_internal_node(size_type e_ind, const const_e_iterator pref_b_it);

      void
      update_prefixes(const_e_access_traits_pointer p_traits);

      const_iterator
      begin() const;

      iterator
      begin();

      const_iterator
      end() const;

      iterator
      end();

      inline node_pointer
      get_child_node(const_e_iterator b_it, const_e_iterator e_it, const_e_access_traits_pointer p_traits);

      inline const_node_pointer
      get_child_node(const_e_iterator b_it, const_e_iterator e_it, const_e_access_traits_pointer p_traits) const;

      inline iterator
      get_child_it(const_e_iterator b_it, const_e_iterator e_it, const_e_access_traits_pointer p_traits);

      inline node_pointer
      get_lower_bound_child_node(const_e_iterator b_it, const_e_iterator e_it, size_type checked_ind, const_e_access_traits_pointer p_traits);

      inline node_pointer
      add_child(node_pointer p_nd, const_e_iterator b_it, const_e_iterator e_it, const_e_access_traits_pointer p_traits);

      inline const_node_pointer
      get_join_child(const_node_pointer p_nd, const_e_access_traits_pointer p_traits) const;

      inline node_pointer
      get_join_child(node_pointer p_nd, const_e_access_traits_pointer p_traits);

      void
      remove_child(node_pointer p_nd);

      iterator
      remove_child(iterator it);

      void
      replace_child(node_pointer p_nd, const_e_iterator b_it, const_e_iterator e_it, const_e_access_traits_pointer p_traits);

      inline const_e_iterator
      pref_b_it() const;

      inline const_e_iterator
      pref_e_it() const;

      inline size_type
      get_e_ind() const;

      bool
      should_be_mine(const_e_iterator b_it, const_e_iterator e_it, size_type checked_ind, const_e_access_traits_pointer p_traits) const;

      leaf_pointer
      leftmost_descendant();

      const_leaf_pointer
      leftmost_descendant() const;

      leaf_pointer
      rightmost_descendant();

      const_leaf_pointer
      rightmost_descendant() const;

#ifdef PB_DS_PAT_TRIE_DEBUG_
      size_type
      e_ind() const;
#endif // #ifdef PB_DS_PAT_TRIE_DEBUG_

    private:
      pat_trie_internal_node(const PB_DS_CLASS_C_DEC& other);

      size_type
      get_begin_pos() const;

    private:
      const size_type m_e_ind;

      const_e_iterator m_pref_b_it;
      const_e_iterator m_pref_e_it;

      node_pointer m_a_p_children[arr_size];

      static leaf_allocator s_leaf_alloc;

      static internal_node_allocator s_internal_node_alloc;
    };

    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::leaf_allocator
    PB_DS_CLASS_C_DEC::s_leaf_alloc;

    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::internal_node_allocator
    PB_DS_CLASS_C_DEC::s_internal_node_alloc;

    PB_DS_CLASS_T_DEC
    inline typename PB_DS_CLASS_C_DEC::size_type
    PB_DS_CLASS_C_DEC::
    get_pref_pos(const_e_iterator b_it, const_e_iterator e_it, const_e_access_traits_pointer p_traits) const
    {
      if (static_cast<size_t>(std::distance(b_it, e_it)) <= m_e_ind)
	return (0);

      std::advance(b_it, m_e_ind);

      return (1 + p_traits->e_pos(*b_it));
    }

    PB_DS_CLASS_T_DEC
    PB_DS_CLASS_C_DEC::
    pat_trie_internal_node(size_type e_ind, const const_e_iterator pref_b_it) :
      PB_DS_BASE_C_DEC(pat_trie_internal_node_type),
      m_e_ind(e_ind),
      m_pref_b_it(pref_b_it),
      m_pref_e_it(pref_b_it)
    {
      std::advance(m_pref_e_it, m_e_ind);

      std::fill(m_a_p_children, m_a_p_children + arr_size,
		static_cast<node_pointer>(NULL));
    }

    PB_DS_CLASS_T_DEC
    void
    PB_DS_CLASS_C_DEC::
    update_prefixes(const_e_access_traits_pointer p_traits)
    {
      node_pointer p_first =* begin();

      if (p_first->m_type == pat_trie_leaf_node_type)
	m_pref_b_it =
	  p_traits->begin(
			  E_Access_Traits::extract_key(
						       static_cast<const_leaf_pointer>(p_first)->value()));
      else
	{
	  PB_DS_DBG_ASSERT(p_first->m_type == pat_trie_internal_node_type);

	  m_pref_b_it = static_cast<internal_node_pointer>(p_first)->pref_b_it();
	}

      m_pref_e_it = m_pref_b_it;

      std::advance(m_pref_e_it, m_e_ind);
    }

    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::const_iterator
    PB_DS_CLASS_C_DEC::
    begin() const
    {
      return (const_iterator(
			     const_cast<node_pointer_pointer>(m_a_p_children) + get_begin_pos(),
			     const_cast<node_pointer_pointer>(m_a_p_children) + arr_size));
    }

    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::iterator
    PB_DS_CLASS_C_DEC::
    begin()
    {
      return (iterator(
		       m_a_p_children + get_begin_pos(),
		       m_a_p_children + arr_size));
    }

    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::const_iterator
    PB_DS_CLASS_C_DEC::
    end() const
    {
      return (const_iterator(
			     const_cast<node_pointer_pointer>(m_a_p_children) + arr_size,
			     const_cast<node_pointer_pointer>(m_a_p_children) + arr_size));
    }

    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::iterator
    PB_DS_CLASS_C_DEC::
    end()
    {
      return (iterator(        m_a_p_children + arr_size, m_a_p_children + arr_size));
    }

    PB_DS_CLASS_T_DEC
    inline typename PB_DS_CLASS_C_DEC::node_pointer
    PB_DS_CLASS_C_DEC::
    get_child_node(const_e_iterator b_it, const_e_iterator e_it, const_e_access_traits_pointer p_traits)
    {
      const size_type i = get_pref_pos(        b_it, e_it, p_traits);

      PB_DS_DBG_ASSERT(i < arr_size);

      return (m_a_p_children[i]);
    }

    PB_DS_CLASS_T_DEC
    inline typename PB_DS_CLASS_C_DEC::iterator
    PB_DS_CLASS_C_DEC::
    get_child_it(const_e_iterator b_it, const_e_iterator e_it, const_e_access_traits_pointer p_traits)
    {
      const size_type i = get_pref_pos(        b_it, e_it, p_traits);

      PB_DS_DBG_ASSERT(i < arr_size);

      PB_DS_DBG_ASSERT(m_a_p_children[i] != NULL);

      return (iterator(m_a_p_children + i, m_a_p_children + i));
    }

    PB_DS_CLASS_T_DEC
    inline typename PB_DS_CLASS_C_DEC::const_node_pointer
    PB_DS_CLASS_C_DEC::
    get_child_node(const_e_iterator b_it, const_e_iterator e_it, const_e_access_traits_pointer p_traits) const
    {
      return (const_cast<node_pointer>(get_child_node(b_it, e_it, p_traits)));
    }

    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::node_pointer
    PB_DS_CLASS_C_DEC::
    get_lower_bound_child_node(const_e_iterator b_it, const_e_iterator e_it, size_type checked_ind, const_e_access_traits_pointer p_traits)
    {
      if (!should_be_mine(b_it, e_it, checked_ind, p_traits))
	{
	  if (p_traits->cmp_prefixes(b_it, e_it, m_pref_b_it, m_pref_e_it, true))
	    return (leftmost_descendant());

	  return (rightmost_descendant());
	}

      size_type i = get_pref_pos(        b_it, e_it, p_traits);

      PB_DS_DBG_ASSERT(i < arr_size);

      if (m_a_p_children[i] != NULL)
	return (m_a_p_children[i]);

      while (++i < arr_size)
	if (m_a_p_children[i] != NULL)
	  {
	    if (m_a_p_children[i]->m_type == pat_trie_leaf_node_type)
	      return (m_a_p_children[i]);

	    PB_DS_DBG_ASSERT(m_a_p_children[i]->m_type ==
			     pat_trie_internal_node_type);

	    return (static_cast<internal_node_pointer>(
						       m_a_p_children[i])->leftmost_descendant());
	  }

      return (rightmost_descendant());
    }

    PB_DS_CLASS_T_DEC
    inline typename PB_DS_CLASS_C_DEC::node_pointer
    PB_DS_CLASS_C_DEC::
    add_child(node_pointer p_nd, const_e_iterator b_it, const_e_iterator e_it, const_e_access_traits_pointer p_traits)
    {
      const size_type i = get_pref_pos(        b_it, e_it, p_traits);

      PB_DS_DBG_ASSERT(i < arr_size);

      if (m_a_p_children[i] == NULL)
	{
	  m_a_p_children[i] = p_nd;

	  p_nd->m_p_parent = this;

	  return (p_nd);
	}

      return (m_a_p_children[i]);
    }

    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::const_node_pointer
    PB_DS_CLASS_C_DEC::
    get_join_child(const_node_pointer p_nd, const_e_access_traits_pointer p_traits) const
    {
      return (const_cast<internal_node_pointer>(this)->get_join_child(
								      const_cast<node_pointer>(p_nd),
								      p_traits));
    }

    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::node_pointer
    PB_DS_CLASS_C_DEC::
    get_join_child(node_pointer p_nd, const_e_access_traits_pointer p_traits)
    {
      size_type i;

      const_e_iterator b_it;
      const_e_iterator e_it;

      if (p_nd->m_type == pat_trie_leaf_node_type)
	{
	  typename Type_Traits::const_key_reference r_key =
	    e_access_traits::extract_key(
					 static_cast<const_leaf_pointer>(p_nd)->value());

	  b_it = p_traits->begin(r_key);
	  e_it = p_traits->end(r_key);
	}
      else
	{
	  b_it = static_cast<internal_node_pointer>(p_nd)->pref_b_it();
	  e_it = static_cast<internal_node_pointer>(p_nd)->pref_e_it();
	}

      i = get_pref_pos(b_it, e_it, p_traits);

      PB_DS_DBG_ASSERT(i < arr_size);

      return (m_a_p_children[i]);
    }

    PB_DS_CLASS_T_DEC
    void
    PB_DS_CLASS_C_DEC::
    remove_child(node_pointer p_nd)
    {
      size_type i = 0;

      for (; i < arr_size; ++i)
	if (m_a_p_children[i] == p_nd)
	  {
	    m_a_p_children[i] = NULL;

	    return;
	  }

      PB_DS_DBG_ASSERT(i != arr_size);
    }

    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::iterator
    PB_DS_CLASS_C_DEC::
    remove_child(iterator it)
    {
      iterator ret = it;

      ++ret;

      * it.m_p_p_cur = NULL;

      return (ret);
    }

    PB_DS_CLASS_T_DEC
    void
    PB_DS_CLASS_C_DEC::
    replace_child(node_pointer p_nd, const_e_iterator b_it, const_e_iterator e_it, const_e_access_traits_pointer p_traits)
    {
      const size_type i = get_pref_pos(        b_it, e_it, p_traits);

      PB_DS_DBG_ASSERT(i < arr_size);

      m_a_p_children[i] = p_nd;

      p_nd->m_p_parent = this;
    }

    PB_DS_CLASS_T_DEC
    inline typename PB_DS_CLASS_C_DEC::const_e_iterator
    PB_DS_CLASS_C_DEC::
    pref_b_it() const
    {
      return (m_pref_b_it);
    }

    PB_DS_CLASS_T_DEC
    inline typename PB_DS_CLASS_C_DEC::const_e_iterator
    PB_DS_CLASS_C_DEC::
    pref_e_it() const
    {
      return (m_pref_e_it);
    }

    PB_DS_CLASS_T_DEC
    inline typename PB_DS_CLASS_C_DEC::size_type
    PB_DS_CLASS_C_DEC::
    get_e_ind() const
    {
      return (m_e_ind);
    }

    PB_DS_CLASS_T_DEC
    bool
    PB_DS_CLASS_C_DEC::
    should_be_mine(const_e_iterator b_it, const_e_iterator e_it, size_type checked_ind, const_e_access_traits_pointer p_traits) const
    {
      if (m_e_ind == 0)
	return (true);

      const size_type num_es = std::distance(b_it, e_it);

      if (num_es < m_e_ind)
	return (false);

      const_e_iterator key_b_it = b_it;
      std::advance(key_b_it, checked_ind);
      const_e_iterator key_e_it = b_it;
      std::advance(key_e_it, m_e_ind);

      const_e_iterator value_b_it = m_pref_b_it;
      std::advance(value_b_it, checked_ind);
      const_e_iterator value_e_it = m_pref_b_it;
      std::advance(value_e_it, m_e_ind);

      return (p_traits->equal_prefixes(            key_b_it, key_e_it, value_b_it, value_e_it));
    }

    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::leaf_pointer
    PB_DS_CLASS_C_DEC::
    leftmost_descendant()
    {
      node_pointer p_pot =* begin();

      if (p_pot->m_type == pat_trie_leaf_node_type)
	return (static_cast<leaf_pointer>(p_pot));

      PB_DS_DBG_ASSERT(p_pot->m_type == pat_trie_internal_node_type);

      return (static_cast<internal_node_pointer>(p_pot)->leftmost_descendant());
    }

    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::const_leaf_pointer
    PB_DS_CLASS_C_DEC::
    leftmost_descendant() const
    {
      return (const_cast<internal_node_pointer>(this)->leftmost_descendant());
    }

    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::leaf_pointer
    PB_DS_CLASS_C_DEC::
    rightmost_descendant()
    {
      const size_type num_children = std::distance(begin(), end());

      PB_DS_DBG_ASSERT(num_children >= 2);

      iterator it = begin();
      std::advance(it, num_children - 1);

      node_pointer p_pot =* it;

      if (p_pot->m_type == pat_trie_leaf_node_type)
	return (static_cast<leaf_pointer>(p_pot));

      PB_DS_DBG_ASSERT(p_pot->m_type == pat_trie_internal_node_type);

      return (static_cast<internal_node_pointer>(p_pot)->rightmost_descendant());
    }

    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::const_leaf_pointer
    PB_DS_CLASS_C_DEC::
    rightmost_descendant() const
    {
      return (const_cast<internal_node_pointer>(this)->rightmost_descendant());
    }

#ifdef PB_DS_PAT_TRIE_DEBUG_
    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::size_type
    PB_DS_CLASS_C_DEC::
    e_ind() const
    {
      return (m_e_ind);
    }
#endif // #ifdef PB_DS_PAT_TRIE_DEBUG_

    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::size_type
    PB_DS_CLASS_C_DEC::
    get_begin_pos() const
    {
      size_type i;

      for (i = 0; i < arr_size&&  m_a_p_children[i] == NULL; ++i)
	;

      return (i);
    }

#ifdef PB_DS_PAT_TRIE_DEBUG_
    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::subtree_debug_info
    PB_DS_CLASS_C_DEC::
    assert_valid_imp(const_e_access_traits_pointer p_traits) const
    {
      PB_DS_DBG_ASSERT(base_type::m_type == pat_trie_internal_node_type);

      PB_DS_DBG_ASSERT(
		       static_cast<size_type>(std::distance(pref_b_it(), pref_e_it())) ==
		       m_e_ind);

      PB_DS_DBG_ASSERT(
		       std::distance(begin(), end()) >= 2);

      for (typename pat_trie_internal_node::const_iterator it = begin();
	   it != end(); ++it)
	{
	  const_node_pointer p_nd =* it;

	  PB_DS_DBG_ASSERT(p_nd->m_p_parent == this);

	  subtree_debug_info child_ret = p_nd->assert_valid_imp(p_traits);

	  PB_DS_DBG_ASSERT(
			   static_cast<size_type>(std::distance(child_ret.first, child_ret.second)) >=
			   m_e_ind);

	  PB_DS_DBG_ASSERT(should_be_mine(child_ret.first, child_ret.second, 0, p_traits));

	  PB_DS_DBG_ASSERT(
			   get_pref_pos(child_ret.first, child_ret.second, p_traits) ==
			   static_cast<size_type>(it.m_p_p_cur - m_a_p_children));
	}

      return (std::make_pair(pref_b_it(), pref_e_it()));
    }
#endif // #ifdef PB_DS_PAT_TRIE_DEBUG_

#undef PB_DS_CLASS_T_DEC

#undef PB_DS_CLASS_C_DEC

#undef PB_DS_BASE_C_DEC

#undef PB_DS_LEAF_C_DEC

#undef PB_DS_STATIC_ASSERT

#undef PB_DS_DBG_ASSERT
#undef PB_DS_DBG_VERIFY
#undef PB_DS_DBG_ONLY

  } // namespace detail
} // namespace pb_ds

#endif // #ifndef PB_DS_PAT_TRIE_NODE_BASE_HPP

