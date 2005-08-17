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
 * @file gp_ht_map_.hpp
 * Contains an implementation class for gp_ht_map_.
 */

#include <ext/pb_assoc/detail/cond_dealtor.hpp>
#include <ext/pb_assoc/trivial_iterator_def.hpp>
#include <ext/pb_assoc/detail/hash_fn/ranged_probe_fn.hpp>
#include <ext/pb_assoc/detail/hash_types_traits.hpp>
#include <ext/pb_assoc/detail/types_traits.hpp>
#include <ext/pb_assoc/exception.hpp>
#include <ext/pb_assoc/detail/map_debug_base.hpp>
#include <ext/pb_assoc/detail/eq_fn/hash_eq_fn.hpp>
#ifdef PB_ASSOC_BASIC_REGRESSION
#include <pb_assoc/testsuite/regression/basic_test/throw_prob_adjustor.hpp>
#endif // #ifdef PB_ASSOC_BASIC_REGRESSION
#include <utility>

namespace pb_assoc
{

  namespace detail
  {

#ifdef PB_ASSOC_GP_HT_MAP_DEBUG_
#define PB_ASSOC_DBG_ASSERT(X) assert(X)
#define PB_ASSOC_DBG_VERIFY(X) assert(X)
#define PB_ASSOC_DBG_ONLY(X) X
#else // #ifdef PB_ASSOC_GP_HT_MAP_DEBUG_
#define PB_ASSOC_DBG_ASSERT(X)
#define PB_ASSOC_DBG_VERIFY(X) {if((X)==0);}
#define PB_ASSOC_DBG_ONLY(X) ;
#endif // #ifdef PB_ASSOC_GP_HT_MAP_DEBUG_

#define PB_ASSOC_CLASS_T_DEC \
	template< \
		typename Key, \
		typename Data, \
		class Hash_Fn, \
		class Eq_Fn, \
		class Allocator, \
		bool Store_Hash, \
		class Comb_Probe_Fn, \
		class Probe_Fn, \
		class Resize_Policy>

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
#define PB_ASSOC_CLASS_NAME \
	gp_ht_map_data_
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

#ifdef PB_ASSOC_DATA_FALSE_INDICATOR
#define PB_ASSOC_CLASS_NAME \
	gp_ht_map_no_data_
#endif // #ifdef PB_ASSOC_DATA_FALSE_INDICATOR

#define PB_ASSOC_CLASS_C_DEC \
	PB_ASSOC_CLASS_NAME< \
		Key, \
		Data, \
		Hash_Fn, \
		Eq_Fn, \
		Allocator, \
		Store_Hash, \
		Comb_Probe_Fn, \
		Probe_Fn, \
		Resize_Policy>

#define PB_ASSOC_HASH_EQ_FN_C_DEC \
	pb_assoc::detail::hash_eq_fn< \
		Key, \
		Eq_Fn, \
		Allocator, \
		Store_Hash>

#define PB_ASSOC_RANGED_PROBE_FN_C_DEC \
	pb_assoc::detail::ranged_probe_fn< \
		Key, \
		Hash_Fn, \
		Allocator, \
		Comb_Probe_Fn, \
		Probe_Fn, \
		Store_Hash>

#define PB_ASSOC_TYPES_TRAITS_C_DEC \
	types_traits< \
		Key, \
		Data, \
		Allocator>

#define PB_ASSOC_HASH_TYPES_TRAITS_C_DEC \
	hash_types_traits< \
		typename Allocator::size_type, \
		Store_Hash>

#ifdef PB_ASSOC_USE_MAP_DEBUG_BASE
#define PB_ASSOC_MAP_DEBUG_BASE_C_DEC \
	pb_assoc::detail::map_debug_base< \
		Key, \
		Eq_Fn>
#endif // #ifdef PB_ASSOC_USE_MAP_DEBUG_BASE

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
#define PB_ASSOC_V2F(X) (X).first
#define PB_ASSOC_V2S(X) (X).second
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

#ifdef PB_ASSOC_DATA_FALSE_INDICATOR
#define PB_ASSOC_V2F(X) (X)
#define PB_ASSOC_V2S(X) Mapped_Data()
#endif // #ifdef PB_ASSOC_DATA_FALSE_INDICATOR

#define PB_ASSOC_STATIC_ASSERT(UNIQUE, E) \
	typedef \
		pb_assoc::detail::static_assert_dummy_class< \
			sizeof(pb_assoc::detail::static_assert<(bool)(E)>)> \
			UNIQUE##static_assert_type

    template<typename Key,
	     typename Data,
	     class Hash_Fn,
	     class Eq_Fn,
	     class Allocator,
	     bool Store_Hash,
	     class Comb_Probe_Fn,
	     class Probe_Fn,
	     class Resize_Policy>
    class PB_ASSOC_CLASS_NAME :
#ifdef PB_ASSOC_GP_HT_MAP_DEBUG_
      protected PB_ASSOC_MAP_DEBUG_BASE_C_DEC,
#endif // #ifdef PB_ASSOC_GP_HT_MAP_DEBUG_
      public PB_ASSOC_HASH_EQ_FN_C_DEC,
      public Resize_Policy,
      public PB_ASSOC_RANGED_PROBE_FN_C_DEC,
      public PB_ASSOC_TYPES_TRAITS_C_DEC,
      public PB_ASSOC_HASH_TYPES_TRAITS_C_DEC
    {

    public:

      typedef typename Allocator::size_type size_type;

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

    protected:

      typedef typename PB_ASSOC_HASH_TYPES_TRAITS_C_DEC::comp_hash comp_hash;

      enum ENTRY_STATUS
	{
	  EMPTY_ENTRY_STATUS,
	  VALID_ENTRY_STATUS,
	  ERASED_ENTRY_STATUS
	};

      typedef char entry_status;

      struct store_hash_entry
      {
	value_type m_value;

	entry_status m_stat;

	size_type m_hash;
      };

      struct no_store_hash_entry
      {
	value_type m_value;

	entry_status m_stat;
      };

      typedef
      typename cond_type<
	Store_Hash,
	store_hash_entry,
	no_store_hash_entry>::type
      entry;

      typedef
      typename Allocator::template rebind<entry>::other
      entry_allocator;

      typedef typename entry_allocator::pointer entry_pointer;

      typedef typename entry_allocator::const_pointer const_entry_pointer;

      typedef typename entry_allocator::reference entry_reference;

      typedef
      typename entry_allocator::const_reference
      const_entry_reference;

      typedef typename entry_allocator::pointer entry_array;

      typedef value_type mapped_value_type;

      typedef pointer mapped_pointer;

      typedef const_pointer const_mapped_pointer;

      typedef reference mapped_reference;

      typedef const_reference const_mapped_reference;

#define PB_ASSOC_GEN_POS size_type

#include <ext/pb_assoc/detail/unordered_iterator/const_find_iterator.hpp>
#include <ext/pb_assoc/detail/unordered_iterator/find_iterator.hpp>
#include <ext/pb_assoc/detail/unordered_iterator/const_iterator.hpp>
#include <ext/pb_assoc/detail/unordered_iterator/iterator.hpp>

#undef PB_ASSOC_GEN_POS

      typedef find_iterator_ find_iterator;

      typedef const_find_iterator_ const_find_iterator;

      typedef iterator_ iterator;

      typedef const_iterator_ const_iterator;

      typedef Hash_Fn hash_fn;

      typedef Eq_Fn eq_fn;

      typedef Allocator allocator;

      typedef Probe_Fn probe_fn;

      typedef Comb_Probe_Fn comb_hash_fn;

      typedef Resize_Policy resize_policy;

    protected:

      PB_ASSOC_CLASS_NAME();

      PB_ASSOC_CLASS_NAME(const PB_ASSOC_CLASS_C_DEC& r_other);

      PB_ASSOC_CLASS_NAME(const Hash_Fn& r_hash_fn);

      PB_ASSOC_CLASS_NAME(const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn);

      PB_ASSOC_CLASS_NAME(const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, const Comb_Probe_Fn& r_comb_probe_fn);

      PB_ASSOC_CLASS_NAME(const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, const Comb_Probe_Fn& r_comb_probe_fn, const Probe_Fn& r_probe_fn);

      PB_ASSOC_CLASS_NAME(const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, const Comb_Probe_Fn& r_comb_probe_fn, const Probe_Fn& r_probe_fn, const Resize_Policy& r_resize_policy);

      template<class It>
      void
      copy_from_range(It first_it, It last_it);

      virtual
      ~PB_ASSOC_CLASS_NAME();

      void
      swap(PB_ASSOC_CLASS_C_DEC& r_other);

      inline size_type
      size() const;

      inline size_type
      max_size() const;

      inline bool
      empty() const;

      Hash_Fn& 
      get_hash_fn();

      const Hash_Fn& 
      get_hash_fn() const;

      Eq_Fn& 
      get_eq_fn();

      const Eq_Fn& 
      get_eq_fn() const;

      Probe_Fn& 
      get_probe_fn();

      const Probe_Fn& 
      get_probe_fn() const;

      Comb_Probe_Fn& 
      get_comb_probe_fn();

      const Comb_Probe_Fn& 
      get_comb_probe_fn() const;

      Resize_Policy& 
      get_resize_policy();

      const Resize_Policy& 
      get_resize_policy() const;

      inline std::pair<find_iterator, bool>
      insert(const_reference r_val);

      inline data_reference
      subscript_imp(const_key_reference r_key);

      inline find_iterator
      find(const_key_reference r_key);

      inline const_find_iterator
      find(const_key_reference r_key) const;

      inline find_iterator
      find_end();

      inline const_find_iterator
      find_end() const;

      inline size_type
      erase(const_key_reference r_key, int_to_type<false>);

      inline size_type
      erase(const_key_reference r_key, int_to_type<true>);

      template<class Pred>
      inline size_type
      erase_if(Pred prd);

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

#ifdef PB_ASSOC_CC_HT_MAP_DEBUG

      virtual void
      assert_valid() const;

#endif // #ifdef PB_ASSOC_GP_HT_MAP_DEBUG_

      virtual void
      do_resize(size_type new_size);

    private:

      typedef PB_ASSOC_TYPES_TRAITS_C_DEC my_traits_base;

      typedef PB_ASSOC_HASH_TYPES_TRAITS_C_DEC my_hash_traits_base;

      typedef PB_ASSOC_RANGED_PROBE_FN_C_DEC my_ranged_probe_fn_base;

#ifdef PB_ASSOC_GP_HT_MAP_DEBUG_
      typedef PB_ASSOC_MAP_DEBUG_BASE_C_DEC my_map_debug_base;
#endif // #ifdef PB_ASSOC_GP_HT_MAP_DEBUG_

      typedef PB_ASSOC_HASH_EQ_FN_C_DEC my_hash_eq_fn_base;

      typedef Resize_Policy my_resize_base;

      friend class iterator_;

      friend class const_iterator_;

    private:

      void
      deallocate_all();

      void
      initialize();

      inline void
      constructor_insert_new_imp(const_reference r_val, size_type pos, int_to_type<true>);

      inline void
      constructor_insert_new_imp(const_reference r_val, size_type pos, int_to_type<false>);

      void
      erase_all_valid_entries(entry_array a_entries_resized, size_type size);

      inline bool
      do_resize_if_needed();

      inline void
      do_resize_if_needed_no_throw();

      void
      resize_imp(entry_array a_entries_resized, size_type old_size);

      inline void
      resize_imp_reassign(entry_pointer p_e, entry_array a_entries_resized, int_to_type<false>);

      inline void
      resize_imp_reassign(entry_pointer p_e, entry_array a_entries_resized, int_to_type<true>);

      inline size_type
      find_ins_pos(const_key_reference r_key, int_to_type<false>);

      inline comp_hash
      find_ins_pos(const_key_reference r_key, int_to_type<true>);

      inline std::pair<find_iterator, bool>
      insert_imp(const_reference r_val, int_to_type<false>);

      inline std::pair<find_iterator, bool>
      insert_imp(const_reference r_val, int_to_type<true>);

      inline pointer
      insert_new_imp(const_reference r_val, size_type pos);

      inline pointer
      insert_new_imp(const_reference r_val, comp_hash& r_pos_hash_pair);

      inline data_reference
      subscript_imp(const_key_reference r_key, int_to_type<false>);

      inline data_reference
      subscript_imp(const_key_reference r_key, int_to_type<true>);

      inline const_data_reference
      const_subscript_imp(const_key_reference r_key) const;

      inline pointer
      find_key_pointer(const_key_reference r_key, int_to_type<false>);

      inline pointer
      find_key_pointer(const_key_reference r_key, int_to_type<true>);

      inline const_data_reference
      const_subscript_imp(const_key_reference r_key, int_to_type<false>) const;

      inline const_data_reference
      const_subscript_imp(const_key_reference r_key, int_to_type<true>) const;

      inline size_type
      erase_in_pos_imp(const_key_reference r_key, const comp_hash& r_pos_hash_pair);

      inline void
      erase_entry(entry_pointer p_e);

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
      void
      inc_it_state(pointer& r_p_value, size_type& r_pos) const;
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

      void
      inc_it_state(const_pointer& r_p_value, size_type& r_pos) const;

      void
      get_start_it_state(const_pointer& r_p_value, size_type& r_pos) const;

      void
      get_start_it_state(pointer& r_p_value, size_type& r_pos);

#ifdef PB_ASSOC_CC_HT_MAP_DEBUG

      void
      assert_entry_array_valid(const entry_array a_entries, int_to_type<false>) const;

      void
      assert_entry_array_valid(const entry_array a_entries, int_to_type<true>) const;

#endif // #ifdef PB_ASSOC_GP_HT_MAP_DEBUG_

    private:
      static entry_allocator s_entry_allocator;

      entry_pointer m_a_entries;

      size_type m_num_e;

      size_type m_num_used_e;

      static iterator s_end_it;

      static const_iterator s_const_end_it;

      enum
	{
	  store_hash_ok =
	  !Store_Hash ||
	  !pb_assoc::detail::is_same_type<
	  Hash_Fn,
	  pb_assoc::null_hash_fn>::value
	};

      PB_ASSOC_STATIC_ASSERT(sth, store_hash_ok);
    };

#include <ext/pb_assoc/detail/gp_ht_map_/constructor_destructor_fn_imps.hpp>
#include <ext/pb_assoc/detail/gp_ht_map_/find_fn_imps.hpp>
#include <ext/pb_assoc/detail/gp_ht_map_/resize_fn_imps.hpp>
#include <ext/pb_assoc/detail/gp_ht_map_/debug_fn_imps.hpp>
#include <ext/pb_assoc/detail/gp_ht_map_/info_fn_imps.hpp>
#include <ext/pb_assoc/detail/gp_ht_map_/policy_access_fn_imps.hpp>
#include <ext/pb_assoc/detail/gp_ht_map_/erase_fn_imps.hpp>
#include <ext/pb_assoc/detail/gp_ht_map_/iterator_fn_imps.hpp>
#include <ext/pb_assoc/detail/gp_ht_map_/insert_fn_imps.hpp>

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_HASH_EQ_FN_C_DEC

#undef PB_ASSOC_RANGED_PROBE_FN_C_DEC

#undef PB_ASSOC_TYPES_TRAITS_C_DEC

#undef PB_ASSOC_HASH_TYPES_TRAITS_C_DEC

#undef PB_ASSOC_MAP_DEBUG_BASE_C_DEC

#undef PB_ASSOC_CLASS_NAME

#undef PB_ASSOC_V2F
#undef PB_ASSOC_V2S

#undef PB_ASSOC_DBG_ASSERT
#undef PB_ASSOC_DBG_VERIFY
#undef PB_ASSOC_DBG_ONLY

#undef PB_ASSOC_STATIC_ASSERT

  } // namespace detail

} // namespace pb_assoc
