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
 * @file ov_tree_map_.hpp
 * Contains an implementation class for ov_tree_.
 */

#include <map>
#include <set>
#include <ext/pb_assoc/trivial_iterator_def.hpp>
#include <ext/pb_assoc/tree_policy.hpp>
#include <ext/pb_assoc/detail/eq_fn/eq_by_less.hpp>
#include <ext/pb_assoc/detail/types_traits.hpp>
#include <ext/pb_assoc/detail/map_debug_base.hpp>
#include <ext/pb_assoc/detail/type_utils.hpp>
#include <ext/pb_assoc/exception.hpp>
#include <utility>
#include <functional>
#include <algorithm>
#include <vector>
#include <cassert>
#ifdef PB_ASSOC_BASIC_REGRESSION
#include <pb_assoc/testsuite/regression/basic_test/throw_prob_adjustor.hpp>
#endif // #ifdef PB_ASSOC_BASIC_REGRESSION

namespace pb_assoc
{

  namespace detail
  {

#ifdef PB_ASSOC_OV_TREE_DEBUG_
#define PB_ASSOC_DBG_ASSERT(X) assert(X);
#define PB_ASSOC_DBG_VERIFY(X) PB_ASSOC_DBG_ASSERT(X)
#define PB_ASSOC_DBG_ONLY(X) X
#else // #ifdef PB_ASSOC_OV_TREE_DEBUG_
#define PB_ASSOC_DBG_ASSERT(X) ((void)0)
#define PB_ASSOC_DBG_VERIFY(X) X
#define PB_ASSOC_DBG_ONLY(X) ;
#endif // #ifdef PB_ASSOC_OV_TREE_DEBUG_

#define PB_ASSOC_CLASS_T_DEC \
	template< \
		typename Key, \
		typename Data, \
		class Cmp_Fn, \
		class Allocator, \
		class Node_Updator>

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
#define PB_ASSOC_OV_TREE_CLASS_NAME \
	ov_tree_data_
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

#ifdef PB_ASSOC_DATA_FALSE_INDICATOR
#define PB_ASSOC_OV_TREE_CLASS_NAME \
	ov_tree_no_data_
#endif // #ifdef PB_ASSOC_DATA_FALSE_INDICATOR

#define PB_ASSOC_CLASS_C_DEC \
	PB_ASSOC_OV_TREE_CLASS_NAME< \
		Key, \
		Data, \
		Cmp_Fn, \
		Allocator, \
		Node_Updator>

#define PB_ASSOC_TYPES_TRAITS_C_DEC \
	types_traits< \
		Key, \
		Data, \
		Allocator>

#ifdef PB_ASSOC_USE_MAP_DEBUG_BASE
#define PB_ASSOC_MAP_DEBUG_BASE_C_DEC \
	pb_assoc::detail::map_debug_base< \
		Key, \
		eq_by_less<Key, Cmp_Fn> >
#endif // #ifdef PB_ASSOC_USE_MAP_DEBUG_BASE

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
#define PB_ASSOC_V2F(X) (X).first
#define PB_ASSOC_V2S(X) (X).second
#define PB_ASSOC_EP2VP(X)& ((X)->m_value)
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

#ifdef PB_ASSOC_DATA_FALSE_INDICATOR
#define PB_ASSOC_V2F(X) (X)
#define PB_ASSOC_V2S(X) Mapped_Data()
#define PB_ASSOC_EP2VP(X)& ((X)->m_value.first)
#endif // #ifdef PB_ASSOC_DATA_FALSE_INDICATOR

    template<typename Key,
	     typename Data,
	     class Cmp_Fn,
	     class Allocator,
	     class Node_Updator>
    class PB_ASSOC_OV_TREE_CLASS_NAME :
#ifdef PB_ASSOC_OV_TREE_DEBUG_
      protected PB_ASSOC_MAP_DEBUG_BASE_C_DEC,
#endif // #ifdef PB_ASSOC_OV_TREE_DEBUG_
      public Cmp_Fn,
      public PB_ASSOC_TYPES_TRAITS_C_DEC,
      public Node_Updator
    {

    protected:

      typedef typename Allocator::size_type size_type;

      typedef typename Allocator::difference_type difference_type;

      typedef
      typename PB_ASSOC_TYPES_TRAITS_C_DEC::const_key_reference
      const_key_reference;

      typedef typename PB_ASSOC_TYPES_TRAITS_C_DEC::data_type data_type;

      typedef
      typename PB_ASSOC_TYPES_TRAITS_C_DEC::data_reference
      data_reference;

      typedef
      typename PB_ASSOC_TYPES_TRAITS_C_DEC::const_data_reference
      const_data_reference;

      typedef typename PB_ASSOC_TYPES_TRAITS_C_DEC::value_type value_type;

      typedef typename PB_ASSOC_TYPES_TRAITS_C_DEC::pointer pointer;

      typedef
      typename PB_ASSOC_TYPES_TRAITS_C_DEC::const_pointer
      const_pointer;

      typedef typename PB_ASSOC_TYPES_TRAITS_C_DEC::reference reference;

      typedef
      typename PB_ASSOC_TYPES_TRAITS_C_DEC::const_reference
      const_reference;

      typedef const_pointer const_find_iterator;

      typedef pointer find_iterator;

      typedef const_find_iterator const_iterator;

      typedef find_iterator iterator;

      typedef pointer value_pointer;

#include <ext/pb_assoc/detail/ov_tree_map_/node_iterators.hpp>

#include <ext/pb_assoc/detail/ov_tree_map_/cond_dtor.hpp>

      typedef Cmp_Fn cmp_fn;

      typedef Allocator allocator;

      typedef PB_ASSOC_TYPES_TRAITS_C_DEC my_traits_base;

      typedef cmp_fn my_cmp_fn_base;

#ifdef PB_ASSOC_USE_MAP_DEBUG_BASE
      typedef PB_ASSOC_MAP_DEBUG_BASE_C_DEC my_map_debug_base;
#endif // #ifdef PB_ASSOC_USE_MAP_DEBUG_BASE

    protected:

      PB_ASSOC_OV_TREE_CLASS_NAME();

      PB_ASSOC_OV_TREE_CLASS_NAME(const Cmp_Fn& r_cmp_fn);

      PB_ASSOC_OV_TREE_CLASS_NAME(const Cmp_Fn& r_cmp_fn, const Node_Updator& r_node_updator);

      PB_ASSOC_OV_TREE_CLASS_NAME(const PB_ASSOC_CLASS_C_DEC& r_other);

      ~PB_ASSOC_OV_TREE_CLASS_NAME();

      void
      swap(PB_ASSOC_CLASS_C_DEC& r_other);

      template<class It>
      void
      copy_from_range(It first_it, It last_it);

      template<class Node_Updator_>
      void
      update(node_iterator it, Node_Updator_* p_updator)
      {
	if (it == node_end())
	  return;

	update(it.l_child(), p_updator);
	update(it.r_child(), p_updator);

	p_updator->operator()(it.m_p_value,(it.l_child() == node_end())? NULL : it.l_child().m_p_value,(it.r_child() == node_end())? NULL : it.r_child().m_p_value);
      }

      inline void
      update(node_iterator /*it*/, pb_assoc::null_node_updator* )
      { }

      bool
      cmp_with_other(const PB_ASSOC_CLASS_C_DEC& r_other) const;

      inline size_type
      max_size() const;

      inline bool
      empty() const;

      inline size_type
      size() const;

      Cmp_Fn& 
      get_cmp_fn();

      const Cmp_Fn& 
      get_cmp_fn() const;

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
      inline data_reference
      subscript_imp(const_key_reference r_key)
      {
	PB_ASSOC_DBG_ONLY(assert_valid();)

	  find_iterator it = lower_bound(r_key);

	if (it != find_end()&&  !Cmp_Fn::operator()(
						    r_key,
						    PB_ASSOC_V2F(*it)))
	  {
	    PB_ASSOC_DBG_ONLY(my_map_debug_base::check_key_exists(r_key));

	    PB_ASSOC_DBG_ONLY(assert_valid();)

	      return (it->second);
	  }

	PB_ASSOC_DBG_ONLY(assert_valid();)

	  return (insert_new_val(it,
				 std::make_pair(r_key, data_type()))->second);
      }

      inline const_data_reference
      subscript_imp(const_key_reference r_key) const
      {
	PB_ASSOC_DBG_ONLY(assert_valid();)

	  PB_ASSOC_DBG_ONLY(my_map_debug_base::check_key_exists(r_key));

	find_iterator it =	 lower_bound(r_key);

	PB_ASSOC_DBG_ASSERT(it != find_end());

	return (it->second);
      }
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

      inline std::pair<find_iterator, bool>
      insert(const_reference r_value)
      {
	PB_ASSOC_DBG_ONLY(assert_valid();)

	  const_key_reference r_key = PB_ASSOC_V2F(r_value);

	find_iterator it = lower_bound(r_key);

	if (it != find_end()&&  !Cmp_Fn::operator()(
						    r_key,
						    PB_ASSOC_V2F(*it)))
	  {
	    PB_ASSOC_DBG_ONLY(assert_valid();)

	      PB_ASSOC_DBG_ONLY(my_map_debug_base::check_key_exists(r_key));

	    return (std::make_pair(it, false));
	  }

	PB_ASSOC_DBG_ONLY(assert_valid();)

	  return (std::make_pair(insert_new_val(it, r_value), true));
      }

      inline static pointer
      mid_pointer(pointer p_begin, pointer p_end)
      {
	PB_ASSOC_DBG_ASSERT(p_end >= p_begin);

	return (p_begin + (p_end - p_begin) / 2);
      }

      inline find_iterator
      lower_bound(const_key_reference r_key)
      {
	pointer it = m_a_values;

	difference_type dist = m_size;

	while (dist > 0)
	  {
	    const difference_type mid_dist  = dist >> 1;

	    pointer mid_it = it + mid_dist;

	    if (my_cmp_fn_base::operator()(
					   PB_ASSOC_V2F(*(it + mid_dist)),
					   r_key))
	      {
		it = ++mid_it;

		dist -= mid_dist + 1;
	      }
	    else
	      dist = mid_dist;
	  }

	return (it);
      }

      inline const_find_iterator
      lower_bound(const_key_reference r_key) const
      {
	return (const_cast<PB_ASSOC_CLASS_C_DEC& >(*this).lower_bound(r_key));
      }

      inline find_iterator
      upper_bound(const_key_reference r_key)
      {
	iterator pot_it = lower_bound(r_key);

	if (pot_it != find_end()&&  !Cmp_Fn::operator()(
							r_key,
							PB_ASSOC_V2F(*pot_it)))
	  {
	    PB_ASSOC_DBG_ONLY(my_map_debug_base::check_key_exists(r_key));

	    return (++pot_it);
	  }

	PB_ASSOC_DBG_ONLY(my_map_debug_base::check_key_does_not_exist(r_key));

	return (pot_it);
      }

      inline const_find_iterator
      upper_bound(const_key_reference r_key) const
      {
	return (const_cast<PB_ASSOC_CLASS_C_DEC& >(*this).upper_bound(r_key));
      }

      inline find_iterator
      find(const_key_reference r_key)
      {
	PB_ASSOC_DBG_ONLY(assert_valid();)

	  iterator pot_it = lower_bound(r_key);

	if (pot_it != find_end()&&  !Cmp_Fn::operator()(
							r_key,
							PB_ASSOC_V2F(*pot_it)))
	  {
	    PB_ASSOC_DBG_ONLY(my_map_debug_base::check_key_exists(r_key));

	    return (pot_it);
	  }

	PB_ASSOC_DBG_ONLY(my_map_debug_base::check_key_does_not_exist(r_key));

	return (find_end());
      }

      inline const_find_iterator
      find(const_key_reference r_key) const
      {
	return (const_cast<PB_ASSOC_CLASS_C_DEC& >(*this).find(r_key));
      }

      inline size_type
      erase(const_key_reference r_key);

      template<class Pred>
      inline size_type
      erase_if(Pred pred);

      template<class It>
      inline It
      erase(It it);

      void
      clear();

      void
      join(PB_ASSOC_CLASS_C_DEC& r_other);

      void
      split(const_key_reference r_key, PB_ASSOC_CLASS_C_DEC& r_other);

      inline iterator
      begin()
      {
	return (m_a_values);
      }

      inline const_iterator
      begin() const
      {
	return (m_a_values);
      }

      inline iterator
      find_end()
      {
	return (end());
      }

      inline const_iterator
      find_end() const
      {
	return (end());
      }

      inline iterator
      end()
      {
	return (m_end_it);
      }

      inline const_iterator
      end() const
      {
	return (m_end_it);
      }

      inline const_node_iterator
      node_begin() const
      {
	return (const_node_iterator(mid_pointer(begin(), end()), begin(), end()));
      }

      inline node_iterator
      node_begin()
      {
	return (node_iterator(mid_pointer(begin(), end()), begin(), end()));
      }

      inline const_node_iterator
      node_end() const
      {
	return (const_node_iterator(end(), end(), end()));
      }

      inline node_iterator
      node_end()
      {
	return (node_iterator(end(), end(), end()));
      }

    private:

      inline pointer
      insert_new_val(iterator it, const_reference r_value)
      {
	PB_ASSOC_DBG_ONLY(assert_valid();)

#ifdef PB_ASSOC_BASIC_REGRESSION
	  throw_prob_adjustor adjust(m_size);
#endif // #ifdef PB_ASSOC_BASIC_REGRESSION

	PB_ASSOC_DBG_ONLY(my_map_debug_base::check_key_does_not_exist(
								      PB_ASSOC_V2F(r_value)));

	pointer a_values = s_alloc.allocate(m_size + 1);

	iterator source_it = begin();
	iterator source_end_it = end();
	iterator target_it = a_values;
	iterator ret_it;

	cond_dtor cd(a_values, target_it, m_size + 1);

	while (source_it != it)
	  {
	    new (const_cast<void* >(
				    static_cast<const void* >(target_it)))
	      value_type(*source_it++);

	    ++target_it;
	  }

	new (const_cast<void* >(
				static_cast<const void* >(ret_it = target_it)))
	  value_type(r_value);

	++target_it;

	while (source_it != source_end_it)
	  {
	    new (const_cast<void* >(
				    static_cast<const void* >(target_it)))
	      value_type(*source_it++);

	    ++target_it;
	  }

	cd.set_no_action();

	if (m_size != 0)
	  {
	    cond_dtor cd1(m_a_values, m_end_it, m_size);
	  }

	++m_size;

	m_a_values = a_values;

	m_end_it = m_a_values + m_size;

	PB_ASSOC_DBG_ONLY(my_map_debug_base::insert_new(
							PB_ASSOC_V2F(r_value)));

	update(node_begin(), (Node_Updator* )this);

	PB_ASSOC_DBG_ONLY(PB_ASSOC_CLASS_C_DEC::assert_valid();)

	  return (ret_it);
      }

#ifdef PB_ASSOC_OV_TREE_DEBUG_

      virtual void
      assert_valid() const;

      void
      assert_iterators() const;

#endif // #ifdef PB_ASSOC_OV_TREE_DEBUG_

      template<class It>
      void
      copy_from_ordered_range(It first_it, It last_it);

      template<class It>
      void
      copy_from_ordered_range(It first_it, It last_it, It other_first_it, It other_last_it);

    private:
      typedef
      typename PB_ASSOC_TYPES_TRAITS_C_DEC::value_type_allocator
      value_allocator;

      pointer m_a_values;

      static value_allocator s_alloc;

      pointer m_end_it;

      size_type m_size;
    };

#include <ext/pb_assoc/detail/ov_tree_map_/constructors_destructor_fn_imps.hpp>
#include <ext/pb_assoc/detail/ov_tree_map_/iterators_fn_imps.hpp>
#include <ext/pb_assoc/detail/ov_tree_map_/debug_fn_imps.hpp>
#include <ext/pb_assoc/detail/ov_tree_map_/erase_fn_imps.hpp>
#include <ext/pb_assoc/detail/ov_tree_map_/insert_fn_imps.hpp>
#include <ext/pb_assoc/detail/ov_tree_map_/find_fn_imps.hpp>
#include <ext/pb_assoc/detail/ov_tree_map_/info_fn_imps.hpp>
#include <ext/pb_assoc/detail/ov_tree_map_/split_join_fn_imps.hpp>

#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_OV_TREE_CLASS_NAME

#undef PB_ASSOC_TYPES_TRAITS_C_DEC

#undef PB_ASSOC_MAP_DEBUG_BASE_C_DEC

#undef PB_ASSOC_V2F
#undef PB_ASSOC_EP2VP
#undef PB_ASSOC_V2S

#undef PB_ASSOC_DBG_ASSERT
#undef PB_ASSOC_DBG_VERIFY
#undef PB_ASSOC_DBG_ONLY

  } // namespace detail

} // namespace pb_assoc
