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

/*
 * @file hash_policy.hpp
 * Contains hash-related policies.
 */

#ifndef HASH_POLICY_HPP
#define HASH_POLICY_HPP

#include <algorithm>
#include <vector>
#include <cmath>
#include <ext/pb_assoc/exception.hpp>
#include <ext/pb_assoc/detail/hash_fn/mask_based_range_hashing.hpp>
#include <ext/pb_assoc/detail/hash_fn/mod_based_range_hashing.hpp>
#include <ext/pb_assoc/detail/resize_policy/size_base.hpp>

namespace pb_assoc
{
  struct null_hash_fn
  { };

  struct null_probe_fn
  { };

#define PB_ASSOC_CLASS_T_DEC \
	template<typename Const_Key_Ref, typename Size_Type>

#define PB_ASSOC_CLASS_C_DEC \
	linear_probe_fn< \
		Const_Key_Ref, \
		Size_Type>

  template<typename Const_Key_Ref, typename Size_Type = size_t>
    class linear_probe_fn
    {
    public:
      typedef Size_Type size_type;
      
      void
      swap(PB_ASSOC_CLASS_C_DEC& r_other);
      
    protected:
      inline size_type
      operator()(Const_Key_Ref r_key, size_type i) const;
    };

#include <ext/pb_assoc/detail/hash_fn/linear_probe_fn_imp.hpp>

#undef PB_ASSOC_CLASS_T_DEC
#undef PB_ASSOC_CLASS_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template<class Const_Key_Ref, typename Size_Type>

#define PB_ASSOC_CLASS_C_DEC \
	quadratic_probe_fn<Const_Key_Ref, Size_Type>

  template<typename Const_Key_Ref, typename Size_Type = size_t>
    class quadratic_probe_fn
    {
    public:
      typedef Size_Type size_type;
      
      void
      swap(PB_ASSOC_CLASS_C_DEC& r_other);
      
    protected:
      inline size_type
      operator()(Const_Key_Ref r_key, size_type i) const;
    };

#include <ext/pb_assoc/detail/hash_fn/quadratic_probe_fn_imp.hpp>

#undef PB_ASSOC_CLASS_T_DEC
#undef PB_ASSOC_CLASS_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template<typename Size_Type>

#define PB_ASSOC_CLASS_C_DEC \
	direct_mask_range_hashing<Size_Type>

  template<typename Size_Type =	size_t>
    class direct_mask_range_hashing 
    : public pb_assoc::detail::mask_based_range_hashing<Size_Type>
    {
    public:
      typedef Size_Type size_type;
      
      void
      swap(PB_ASSOC_CLASS_C_DEC& r_other);
      
    protected:
      void
      notify_resized(size_type size);
      
      inline size_type
      operator()(size_type hash) const;
      
    private:
      typedef pb_assoc::detail::mask_based_range_hashing<Size_Type>
      my_mask_based_base;
    };

#define PB_ASSOC_MASK_BASED_C_DEC \
	pb_assoc::detail::mask_based_range_hashing< \
		Size_Type>

#include <ext/pb_assoc/detail/hash_fn/direct_mask_range_hashing_imp.hpp>

#undef PB_ASSOC_CLASS_T_DEC
#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_MASK_BASED_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template<typename Size_Type>

#define PB_ASSOC_CLASS_C_DEC \
	direct_mod_range_hashing<Size_Type>

#define PB_ASSOC_MOD_BASED_C_DEC \
	pb_assoc::detail::mod_based_range_hashing<Size_Type>

  template<typename Size_Type =	size_t>
    class direct_mod_range_hashing : public PB_ASSOC_MOD_BASED_C_DEC
    {
    public:
      typedef Size_Type size_type;
      
      void
      swap(PB_ASSOC_CLASS_C_DEC& r_other);
      
    protected:
      /*
       *   description = "Notifies the policy object that the container's
       *	  __size has changed to size.">
       **/
      void
      notify_resized(size_type size);
      
      inline size_type
    operator()(size_type hash) const;
      
    private:
      typedef PB_ASSOC_MOD_BASED_C_DEC my_mod_based_base;
    };

#include <ext/pb_assoc/detail/hash_fn/direct_mod_range_hashing_imp.hpp>

#undef PB_ASSOC_CLASS_T_DEC
#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_MOD_BASED_C_DEC

#ifdef PB_ASSOC_HT_LOAD_CHECK_RESIZE_TRIGGER_DEBUG
#define PB_ASSOC_DBG_ASSERT(X) assert(X)
#define PB_ASSOC_DBG_VERIFY(X) assert(X)
#define PB_ASSOC_DBG_ONLY(X) X
#else // #ifdef PB_ASSOC_HT_LOAD_CHECK_RESIZE_TRIGGER_DEBUG
#define PB_ASSOC_DBG_ASSERT(X)
#define PB_ASSOC_DBG_VERIFY(X) {if((X)==0);}
#define PB_ASSOC_DBG_ONLY(X) ;
#endif // #ifdef PB_ASSOC_HT_LOAD_CHECK_RESIZE_TRIGGER_DEBUG

#define PB_ASSOC_CLASS_T_DEC \
	template<bool External_Load_Access, typename Size_Type>

#define PB_ASSOC_CLASS_C_DEC \
	hash_load_check_resize_trigger<External_Load_Access, Size_Type>

#define PB_ASSOC_SIZE_BASE_C_DEC \
	pb_assoc::detail::size_base<Size_Type, External_Load_Access>

  template<bool External_Load_Access = false, typename Size_Type = size_t>
    class hash_load_check_resize_trigger : private PB_ASSOC_SIZE_BASE_C_DEC
    {
    public:
      typedef Size_Type size_type;
      
      enum
	{
	  external_load_access = External_Load_Access
	};
            
      hash_load_check_resize_trigger(float load_min = 0.125, 
				     float load_max = 0.5);

      void
      swap(PB_ASSOC_CLASS_C_DEC& r_other);
      
      virtual
      ~hash_load_check_resize_trigger();
      
      inline std::pair<float, float>
      get_loads() const;
      
      void
      set_loads(std::pair<float, float> load_pair);

    protected:
      inline void
      notify_insert_search_start();
      
      inline void
      notify_insert_search_collision();

      inline void
      notify_insert_search_end();
      
      inline void
      notify_find_search_start();
      
      inline void
      notify_find_search_collision();
      
      inline void
      notify_find_search_end();
      
      inline void
      notify_erase_search_start();
      
      inline void
      notify_erase_search_collision();
      
      inline void
      notify_erase_search_end();
      
      inline void
      notify_inserted(size_type num_entries);
      
      inline void
      notify_erased(size_type num_entries);
      
      void
      notify_cleared();
      
      void
      notify_resized(size_type new_size);
      
      void
      notify_externally_resized(size_type new_size);
      
      inline bool
      is_resize_needed() const;
      
      inline bool
      is_grow_needed(size_type size, size_type num_entries) const;
      
      inline bool
      is_shrink_needed(size_type size, size_type num_entries) const;
      
      typedef PB_ASSOC_SIZE_BASE_C_DEC my_size_base;
      
    private:
      inline std::pair<float, float>
      get_loads_imp(pb_assoc::detail::int_to_type<true>) const;
      
      void
      set_loads_imp(std::pair<float, float>, 
		    pb_assoc::detail::int_to_type<true>);

      virtual void
      do_resize(size_type new_size);
      
#ifdef PB_ASSOC_HT_LOAD_CHECK_RESIZE_TRIGGER_DEBUG
      void
      assert_valid() const;
#endif // #ifdef PB_ASSOC_HT_LOAD_CHECK_RESIZE_TRIGGER_DEBUG

      float m_load_min, m_load_max;
      
      size_type m_next_shrink_size;
      
      size_type m_next_grow_size;
      
      bool m_resize_needed;
      
      static pb_assoc::detail::int_to_type<External_Load_Access>
      s_external_load_access_ind;
    };

#include <ext/pb_assoc/detail/resize_policy/hash_load_check_resize_trigger_imp.hpp>

#undef PB_ASSOC_CLASS_T_DEC
#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_SIZE_BASE_C_DEC

#undef PB_ASSOC_DBG_ASSERT
#undef PB_ASSOC_DBG_VERIFY
#undef PB_ASSOC_DBG_ONLY

#ifdef PB_ASSOC_HT_MAX_COLLISION_CHECK_RESIZE_TRIGGER_POLICY_DEBUG
#define PB_ASSOC_DBG_ASSERT(X) assert(X)
#define PB_ASSOC_DBG_VERIFY(X) assert(X)
#define PB_ASSOC_DBG_ONLY(X) X
#else // #ifdef PB_ASSOC_HT_MAX_COLLISION_CHECK_RESIZE_TRIGGER_POLICY_DEBUG
#define PB_ASSOC_DBG_ASSERT(X)
#define PB_ASSOC_DBG_VERIFY(X) {if((X)==0);}
#define PB_ASSOC_DBG_ONLY(X) ;
#endif // #ifdef PB_ASSOC_HT_MAX_COLLISION_CHECK_RESIZE_TRIGGER_POLICY_DEBUG

#define PB_ASSOC_CLASS_T_DEC \
	template<bool External_Load_Access, typename Size_Type>

#define PB_ASSOC_CLASS_C_DEC \
	cc_hash_max_collision_check_resize_trigger< \
		External_Load_Access, \
		Size_Type>

  template<bool External_Load_Access = false, typename Size_Type = size_t>
    class cc_hash_max_collision_check_resize_trigger
    {
    public:
      typedef Size_Type size_type;
      
      enum
	{
	  external_load_access = External_Load_Access
	};
      
      cc_hash_max_collision_check_resize_trigger(float load = 0.5);
      
      void
      swap(PB_ASSOC_CLASS_C_DEC& r_other);
      
      inline float
      get_load() const;
      
    protected:
      inline void
      notify_insert_search_start();

      inline void
      notify_insert_search_collision();

      inline void
      notify_insert_search_end();

      inline void
      notify_find_search_start();

      inline void
      notify_find_search_collision();

      inline void
      notify_find_search_end();

      inline void
      notify_erase_search_start();

      inline void
      notify_erase_search_collision();

      inline void
      notify_erase_search_end();

      inline void
      notify_inserted(size_type num_entries);

      inline void
      notify_erased(size_type num_entries);

      void
      notify_cleared();

      void
      notify_resized(size_type new_size);

      void
      notify_externally_resized(size_type new_size);

      inline bool
      is_resize_needed() const;

      inline bool
      is_grow_needed(size_type size, size_type num_entries) const;

      inline bool
      is_shrink_needed(size_type size, size_type num_entries) const;

    private:
      template<typename Key>
      class max_col_checker
      {
      public:
	max_col_checker(size_type size, size_type* p_max_col) 
	: m_p_max_col(p_max_col), m_a_col(size, 0)
	{ }

	void
	operator()(const std::pair<const Key, size_type>& r_key_pos_pair)
	{ ++m_a_col[r_key_pos_pair.second]; }

      private:
	std::vector<size_type> m_a_col;
	
	size_type* const m_p_max_col;
      };
      
    private:
      inline float
      get_load_imp(pb_assoc::detail::int_to_type<true>) const;
      
      float m_load;
      
      size_type m_size;
      
      size_type m_num_col;
      
      size_type m_max_col;
      
      bool m_resize_needed;
      
      static pb_assoc::detail::int_to_type<External_Load_Access>
      s_external_load_access_ind;
    };

#include <ext/pb_assoc/detail/resize_policy/cc_hash_max_collision_resize_trigger_imp.hpp>

#undef PB_ASSOC_CLASS_T_DEC
#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_DBG_ASSERT
#undef PB_ASSOC_DBG_VERIFY
#undef PB_ASSOC_DBG_ONLY

#define PB_ASSOC_CLASS_T_DEC \
	template<typename Size_Type>

#define PB_ASSOC_CLASS_C_DEC \
	hash_exponential_size_policy< \
		Size_Type>

  template<typename Size_Type =	size_t>
  class hash_exponential_size_policy
  {
  public:
    typedef Size_Type size_type;

    hash_exponential_size_policy(size_type start_size = 8, 
				 size_type grow_factor = 2);

    void
    swap(PB_ASSOC_CLASS_C_DEC& r_other);

  protected:
    size_type
    get_init_size(size_type suggested_size) const;

    size_type
    get_nearest_larger_size(size_type cur_size) const;

    size_type
    get_nearest_smaller_size(size_type cur_size) const;

#ifdef PB_ASSOC_HT_EXPONENTIAL_SIZE_POLICY_DEBUG
    void
    assert_is_one_of_my_sizes(size_type size) const;
#endif // #ifdef PB_ASSOC_HT_EXPONENTIAL_SIZE_POLICY_DEBUG

  private:
    size_type m_start_size;
    size_type m_grow_factor;
  };

#include <ext/pb_assoc/detail/resize_policy/hash_exponential_size_policy_imp.hpp>

#undef PB_ASSOC_CLASS_T_DEC
#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_DBG_ASSERT
#undef PB_ASSOC_DBG_VERIFY
#undef PB_ASSOC_DBG_ONLY

#define PB_ASSOC_CLASS_T_DEC

#define PB_ASSOC_CLASS_C_DEC \
	hash_prime_size_policy

#ifdef PB_ASSOC_HT_PRIME_SIZE_POLICY_DEBUG
#define PB_ASSOC_DBG_ASSERT(X) assert(X)
#define PB_ASSOC_DBG_VERIFY(X) assert(X)
#define PB_ASSOC_DBG_ONLY(X) X
#else // #ifdef PB_ASSOC_HT_PRIME_SIZE_POLICY_DEBUG
#define PB_ASSOC_DBG_ASSERT(X)
#define PB_ASSOC_DBG_VERIFY(X) {if((X)==0);}
#define PB_ASSOC_DBG_ONLY(X) ;
#endif // #ifdef PB_ASSOC_HT_PRIME_SIZE_POLICY_DEBUG

  struct hash_prime_size_policy
  {
    typedef size_t size_type;

    inline void
    swap(PB_ASSOC_CLASS_C_DEC& r_other);

  protected:
    inline size_type
    get_init_size(size_type suggested_size) const;
    
    inline size_type
    get_nearest_larger_size(size_type cur_size) const;
    
    inline size_type
    get_nearest_smaller_size(size_type cur_size) const;
    
    inline size_type
    get_nearest_larger_size_imp(size_type size) const;
    
#ifdef PB_ASSOC_HT_PRIME_SIZE_POLICY_DEBUG
    void
    assert_is_one_of_my_sizes(size_type size) const;
#endif // #ifdef PB_ASSOC_HT_PRIME_SIZE_POLICY_DEBUG
  };

#include <ext/pb_assoc/detail/resize_policy/hash_prime_size_policy_imp.hpp>

#undef PB_ASSOC_CLASS_T_DEC
#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_DBG_ASSERT
#undef PB_ASSOC_DBG_VERIFY
#undef PB_ASSOC_DBG_ONLY

#define PB_ASSOC_CLASS_T_DEC \
	template< \
		class Size_Policy, \
		class Trigger_Policy, \
		bool External_Size_Access, \
		typename Size_Type>

#define PB_ASSOC_CLASS_C_DEC \
	hash_standard_resize_policy< \
		Size_Policy, \
		Trigger_Policy, \
		External_Size_Access, \
		Size_Type>

  template<class Size_Policy =	pb_assoc::hash_exponential_size_policy<>,
	   class Trigger_Policy = pb_assoc::hash_load_check_resize_trigger<>,
	   bool External_Size_Access = false,
	   typename Size_Type = size_t>
  class hash_standard_resize_policy : public Size_Policy, public Trigger_Policy
  {
  public:
    typedef Size_Type 		size_type;
    typedef Trigger_Policy 	trigger_policy;
    typedef Size_Policy 	size_policy;

    enum
      {
	external_size_access = External_Size_Access
      };

    hash_standard_resize_policy(size_type suggested_size = 8);
    
    hash_standard_resize_policy(const Size_Policy&, 
				size_type suggested_size = 8);

    hash_standard_resize_policy(const Size_Policy&, const Trigger_Policy&, 
				size_type suggested_size = 8);

    virtual
    ~hash_standard_resize_policy();

    inline void
    swap(PB_ASSOC_CLASS_C_DEC& r_other);

    Size_Policy& 
    get_size_policy();

    const Size_Policy& 
    get_size_policy() const;

    Trigger_Policy& 
    get_trigger_policy();

    const Trigger_Policy& 
    get_trigger_policy() const;

    inline size_type
    get_actual_size() const;

    void
    resize(size_type suggested_new_size);

  protected:

    inline void
    notify_insert_search_start();

    inline void
    notify_insert_search_collision();

    inline void
    notify_insert_search_end();

    inline void
    notify_find_search_start();

    inline void
    notify_find_search_collision();

    inline void
    notify_find_search_end();

    inline void
    notify_erase_search_start();

    inline void
    notify_erase_search_collision();

    inline void
    notify_erase_search_end();

    inline void
    notify_inserted(size_type num_e);

    inline void
    notify_erased(size_type num_e);

    void
    notify_cleared();

    void
    notify_resized(size_type new_size);

    size_type
    get_init_size() const;

    inline bool
    is_resize_needed() const;

    size_type
    get_new_size(size_type size, size_type num_used_e) const;

  private:
    typedef Trigger_Policy my_trigger_policy_base;

    typedef Size_Policy my_size_policy_base;

    typedef
    pb_assoc::detail::int_to_type<false>
    external_resize_false_indicator;

    typedef
    pb_assoc::detail::int_to_type<true>
    external_resize_true_indicator;

    inline size_type
    get_actual_size(external_resize_true_indicator) const;

    void
    resize(size_type new_size, external_resize_true_indicator);

    virtual void
    do_resize(size_type new_size);

    static pb_assoc::detail::int_to_type<External_Size_Access>
    s_external_size_access_indicator;

    size_type m_size;
  };

#include <ext/pb_assoc/detail/resize_policy/hash_standard_resize_policy_imp.hpp>

#undef PB_ASSOC_CLASS_T_DEC
#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_DBG_ASSERT
#undef PB_ASSOC_DBG_VERIFY
#undef PB_ASSOC_DBG_ONLY

} // namespace pb_assoc

#endif // #ifndef HASH_POLICY_HPP
