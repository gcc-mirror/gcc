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
 * @file lu_map_.hpp
 * Contains a list update map.
 */

#include <utility>
#include <iterator>
#include <ext/pb_assoc/detail/cond_dealtor.hpp>
#include <ext/pb_assoc/trivial_iterator_def.hpp>
#include <ext/pb_assoc/detail/types_traits.hpp>
#include <ext/pb_assoc/exception.hpp>
#include <ext/pb_assoc/detail/map_debug_base.hpp>

namespace pb_assoc
{

  namespace detail
  {

#ifdef PB_ASSOC_LU_MAP_DEBUG_
#define PB_ASSOC_DBG_ASSERT(X) assert(X)
#define PB_ASSOC_DBG_VERIFY(X) assert(X)
#define PB_ASSOC_DBG_ONLY(X) X
#else // #ifdef PB_ASSOC_LU_MAP_DEBUG_
#define PB_ASSOC_DBG_ASSERT(X)
#define PB_ASSOC_DBG_VERIFY(X) {if((X)==0);}
#define PB_ASSOC_DBG_ONLY(X) ;
#endif // #ifdef PB_ASSOC_LU_MAP_DEBUG_

#define PB_ASSOC_CLASS_T_DEC \
	template< \
		typename Key, \
		typename Data, \
		class Eq_Fn, \
		class Allocator, \
		class Update_Policy>

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
#define PB_ASSOC_CLASS_NAME \
	lu_map_data_
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

#ifdef PB_ASSOC_DATA_FALSE_INDICATOR
#define PB_ASSOC_CLASS_NAME \
	lu_map_no_data_
#endif // #ifdef PB_ASSOC_DATA_FALSE_INDICATOR

#define PB_ASSOC_CLASS_C_DEC \
	PB_ASSOC_CLASS_NAME< \
		Key, \
		Data, \
		Eq_Fn, \
		Allocator, \
		Update_Policy>

#define PB_ASSOC_TYPES_TRAITS_C_DEC \
	pb_assoc::detail::types_traits< \
		Key, \
		Data, \
		Allocator>

#ifdef PB_ASSOC_USE_MAP_DEBUG_BASE
#define PB_ASSOC_MAP_DEBUG_BASE_C_DEC \
	pb_assoc::detail::map_debug_base< \
		Key, \
		Eq_Fn>
#endif // #ifdef PB_ASSOC_USE_MAP_DEBUG_BASE

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
#define PB_ASSOC_V2F(X) (X).first
#define PB_ASSOC_V2S(X) (X).second
#define PB_ASSOC_EP2VP(X)& ((X)->m_value)
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

#ifdef PB_ASSOC_DATA_FALSE_INDICATOR
#define PB_ASSOC_V2F(X) (X)
#define PB_ASSOC_V2S(X) Data()
#define PB_ASSOC_EP2VP(X)& ((X)->m_value.first)
#endif // #ifdef PB_ASSOC_DATA_FALSE_INDICATOR

#ifdef PB_ASSOC_LU_MAP_DEBUG_
#define PB_ASSOC_DBG_ASSERT(X) assert(X)
#define PB_ASSOC_DBG_VERIFY(X) assert(X)
#define PB_ASSOC_DBG_ONLY(X) X
#else // #ifdef PB_ASSOC_LU_MAP_DEBUG_
#define PB_ASSOC_DBG_ASSERT(X)
#define PB_ASSOC_DBG_VERIFY(X) {if((X)==0);}
#define PB_ASSOC_DBG_ONLY(X) ;
#endif // #ifdef PB_ASSOC_LU_MAP_DEBUG_

    /* Skip to the lu, my darling. */

    template<typename Key,
	     typename Data,
	     class Eq_Fn,
	     class Allocator,
	     class Update_Policy>
    class PB_ASSOC_CLASS_NAME :
#ifdef PB_ASSOC_LU_MAP_DEBUG_
      protected PB_ASSOC_MAP_DEBUG_BASE_C_DEC,
#endif // #ifdef PB_ASSOC_LU_MAP_DEBUG_
      public Eq_Fn,
      public Update_Policy,
      public PB_ASSOC_TYPES_TRAITS_C_DEC
    {

    protected:

      typedef typename Allocator::size_type size_type;

      typedef
      typename PB_ASSOC_TYPES_TRAITS_C_DEC::const_key_reference
      const_key_reference;

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR

      typedef typename PB_ASSOC_TYPES_TRAITS_C_DEC::data_type data_type;

      typedef
      typename PB_ASSOC_TYPES_TRAITS_C_DEC::data_reference
      data_reference;

      typedef
      typename PB_ASSOC_TYPES_TRAITS_C_DEC::const_data_reference
      const_data_reference;

#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

      typedef typename PB_ASSOC_TYPES_TRAITS_C_DEC::value_type value_type;

      typedef typename PB_ASSOC_TYPES_TRAITS_C_DEC::pointer pointer;

      typedef
      typename PB_ASSOC_TYPES_TRAITS_C_DEC::const_pointer
      const_pointer;

      typedef typename PB_ASSOC_TYPES_TRAITS_C_DEC::reference reference;

      typedef
      typename PB_ASSOC_TYPES_TRAITS_C_DEC::const_reference
      const_reference;

      typedef Update_Policy update_policy;

      typedef typename Update_Policy::metadata_type update_metadata;

      struct entry
      {
	typename PB_ASSOC_TYPES_TRAITS_C_DEC::value_type m_value;

	update_metadata m_update_metadata;

	typename Allocator::template rebind<entry>::other::pointer m_p_next;
      };

      typedef
      typename Allocator::template rebind<entry>::other
      entry_allocator;

      typedef typename entry_allocator::pointer entry_pointer;

      typedef typename entry_allocator::const_pointer const_entry_pointer;

      typedef typename entry_allocator::reference entry_reference;

      typedef
      typename entry_allocator::const_reference
      const_entry_reference;

      typedef
      typename Allocator::template rebind<entry_pointer>::other
      entry_pointer_allocator;

      typedef typename entry_pointer_allocator::pointer entry_pointer_array;

#define PB_ASSOC_GEN_POS entry_pointer

      typedef value_type mapped_value_type;

      typedef pointer mapped_pointer;

      typedef const_pointer const_mapped_pointer;

      typedef reference mapped_reference;

      typedef const_reference const_mapped_reference;

#include <ext/pb_assoc/detail/unordered_iterator/const_find_iterator.hpp>
#include <ext/pb_assoc/detail/unordered_iterator/find_iterator.hpp>
#include <ext/pb_assoc/detail/unordered_iterator/const_iterator.hpp>
#include <ext/pb_assoc/detail/unordered_iterator/iterator.hpp>

#undef PB_ASSOC_GEN_POS

      typedef find_iterator_ find_iterator;

      typedef const_find_iterator_ const_find_iterator;

      typedef iterator_ iterator;

      typedef const_iterator_ const_iterator;

      typedef Eq_Fn eq_fn;

      typedef Allocator allocator;

    protected:

      PB_ASSOC_CLASS_NAME();

      PB_ASSOC_CLASS_NAME(const Eq_Fn& r_eq_fn);

      PB_ASSOC_CLASS_NAME(const Eq_Fn& r_eq_fn, const Update_Policy& r_update_policy);

      PB_ASSOC_CLASS_NAME(const PB_ASSOC_CLASS_C_DEC& r_other);

      virtual
      ~PB_ASSOC_CLASS_NAME();

      void
      swap(PB_ASSOC_CLASS_C_DEC& r_other);

      template<class It>
      void
      copy_from_range(It first_it, It last_it);

      inline size_type
      size() const;

      inline size_type
      max_size() const;

      inline bool
      empty() const;

      Eq_Fn& 
      get_eq_fn();

      const Eq_Fn& 
      get_eq_fn() const;

      inline std::pair<find_iterator, bool>
      insert(const_reference r_val);

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
      inline data_reference
      subscript_imp(const_key_reference r_key);
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

      inline find_iterator
      find(const_key_reference r_key);

      inline const_find_iterator
      find(const_key_reference r_key) const;

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
      inline const_data_reference
      const_subscript_imp(const_key_reference r_key) const;
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

      inline size_type
      erase(const_key_reference r_key);

      template<class Pred>
      inline size_type
      erase_if(Pred& r_pred);

      void
      clear();

      inline iterator
      begin();

      inline const_iterator
      begin() const;

      inline iterator
      end();

      inline const_iterator
      end() const;

#ifdef PB_ASSOC_LU_MAP_DEBUG_

      virtual void
      assert_valid() const;

#endif // #ifdef PB_ASSOC_LU_MAP_DEBUG_

    private:

      typedef PB_ASSOC_TYPES_TRAITS_C_DEC my_traits_base;

#ifdef PB_ASSOC_USE_MAP_DEBUG_BASE
      typedef PB_ASSOC_MAP_DEBUG_BASE_C_DEC my_map_debug_base;
#endif // #ifdef PB_ASSOC_USE_MAP_DEBUG_BASE

      typedef
      pb_assoc::detail::cond_dealtor<
	entry,
	Allocator>
      cond_dealtor_t;

    private:

      void
      deallocate_all(bool deallocate_root);

      inline void
      move_next_to_front(entry_pointer p_l) const;

      void
      initialize();

      inline void
      insert_new_after(entry_pointer p_l, const_reference r_val);

      inline entry_pointer
      find_imp(const_key_reference r_key) const
      {
	entry_pointer p_l = m_p_l;

	while (p_l->m_p_next != NULL)
	  if (Eq_Fn::operator()(
				r_key,
				PB_ASSOC_V2F(p_l->m_p_next->m_value)))
	    {
	      if (Update_Policy::operator()(p_l->m_update_metadata))
		{
		  move_next_to_front(p_l);

		  return (m_p_l);
		}
	      else
		return (p_l);
	    }
	  else
	    p_l = p_l->m_p_next;

	return (p_l);
      }

      inline void
      erase_imp(entry_pointer p_l);

      inline find_iterator
      find_end();

      inline const_find_iterator
      find_end() const;

      void
      inc_it_state(pointer& r_p_value, std::pair<entry_pointer, size_type>& r_pos) const;

      void
      inc_it_state(const_pointer& r_p_value, entry_pointer& r_pos) const;

      void
      get_start_it_state(pointer& r_p_value, std::pair<entry_pointer, size_type>& r_pos) const;

#ifdef PB_ASSOC_LU_MAP_DEBUG_

      void
      assert_entry_pointer_array_valid(const entry_pointer_array a_p_lntries) const;

      void
      assert_entry_pointer_valid(const entry_pointer p_l, store_hash_true_indicator) const;

      void
      assert_entry_pointer_valid(const entry_pointer p_l, store_hash_false_indicator) const;

#endif // #ifdef PB_ASSOC_LU_MAP_DEBUG_

    private:

      static entry_allocator s_entry_allocator;

      mutable entry_pointer m_p_l;

      size_type m_size;

      friend class iterator_;

      friend class const_iterator_;

      static iterator s_end_it;

      static const_iterator s_const_end_it;

      static find_iterator s_find_end_it;

      static const_find_iterator s_const_find_end_it;

    };

#include <ext/pb_assoc/detail/lu_map_/constructor_destructor_fn_imps.hpp>
#include <ext/pb_assoc/detail/lu_map_/info_fn_imps.hpp>
#include <ext/pb_assoc/detail/lu_map_/debug_fn_imps.hpp>
#include <ext/pb_assoc/detail/lu_map_/policy_access_fn_imps.hpp>
#include <ext/pb_assoc/detail/lu_map_/iterators_fn_imps.hpp>
#include <ext/pb_assoc/detail/lu_map_/erase_fn_imps.hpp>
#include <ext/pb_assoc/detail/lu_map_/find_fn_imps.hpp>
#include <ext/pb_assoc/detail/lu_map_/insert_fn_imps.hpp>

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_TYPES_TRAITS_C_DEC

#undef PB_ASSOC_MAP_DEBUG_BASE_C_DEC

#undef PB_ASSOC_CLASS_NAME

#undef PB_ASSOC_V2F
#undef PB_ASSOC_EP2VP
#undef PB_ASSOC_V2S

#undef PB_ASSOC_DBG_ASSERT
#undef PB_ASSOC_DBG_VERIFY
#undef PB_ASSOC_DBG_ONLY

  } // namespace detail

} // namespace pb_assoc
