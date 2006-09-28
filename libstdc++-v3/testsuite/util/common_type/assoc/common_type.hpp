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
 * @file common_type.hpp
 * Contains common types.
 */

#ifndef PB_DS_COMMON_TYPES_HPP
#define PB_DS_COMMON_TYPES_HPP

#include <ext/pb_ds/detail/type_utils.hpp>
#include <common_type/assoc/template_policy.hpp>
#include <ext/pb_ds/assoc_container.hpp>

namespace pb_ds
{
  namespace test
  {
    template<typename Key,
	     typename Data,
	     class Hash_Fn = typename pb_ds::detail::default_hash_fn<Key>::type,
	     class Eq_Fn = std::equal_to<Key>,
	     class Allocator = std::allocator<std::pair<const Key, Data> > >
    struct hash_common_types
    {
    private:
      typedef typename Allocator::size_type size_type;

      typedef
      pb_ds::test::hash_load_check_resize_trigger_t_<
	Allocator,
	1, 8,
	1, 2,
	false>
      no_access_half_load_check_resize_trigger_policy;

      typedef
      pb_ds::test::hash_load_check_resize_trigger_t_<
	Allocator,
	1, 8,
	1, 2,
	true>
      access_half_load_check_resize_trigger_policy;

      typedef
      pb_ds::test::hash_load_check_resize_trigger_t_<
	Allocator,
	1, 8,
	1, 1,
	false>
      no_access_one_load_check_resize_trigger_policy;

      typedef
      pb_ds::test::cc_hash_max_collision_check_resize_trigger_t_<
	Allocator,
	1, 2,
	false>
      no_access_half_max_col_check_check_resize_trigger_policy;

      typedef
      pb_ds::test::cc_hash_max_collision_check_resize_trigger_t_<
	Allocator,
	1, 2,
	true>
      access_half_max_col_check_check_resize_trigger_policy;

      typedef pb_ds::test::linear_probe_fn_t_<Key, Allocator> lin_p_t;

      typedef pb_ds::test::quadratic_probe_fn_t_<Key, Allocator> quad_p_t;

      typedef
      typename __gnu_cxx::typelist::create4<
	pb_ds::detail::false_type,
	pb_ds::test::direct_mask_range_hashing_t_<
	Allocator>,
	no_access_half_load_check_resize_trigger_policy,
	pb_ds::test::hash_exponential_size_policy_t_<
	Allocator> >::type
      performance_cc_policy0;

      typedef
      typename __gnu_cxx::typelist::create4<
	pb_ds::detail::false_type,
	pb_ds::test::direct_mod_range_hashing_t_<
	Allocator>,
	no_access_half_load_check_resize_trigger_policy,
	pb_ds::test::hash_prime_size_policy_t_>::type
      performance_cc_policy1;

      typedef
      typename __gnu_cxx::typelist::create4<
	pb_ds::detail::false_type,
	pb_ds::test::direct_mask_range_hashing_t_<
	Allocator>,
	no_access_one_load_check_resize_trigger_policy,
	pb_ds::test::hash_exponential_size_policy_t_<
	Allocator> >::type
      performance_cc_policy2;

      typedef
      typename __gnu_cxx::typelist::create4<
	pb_ds::detail::false_type,
	pb_ds::test::direct_mod_range_hashing_t_<
	Allocator>,
	no_access_one_load_check_resize_trigger_policy,
	pb_ds::test::hash_prime_size_policy_t_ >::type
      performance_cc_policy3;

      typedef
      typename __gnu_cxx::typelist::create4<
	pb_ds::detail::true_type,
	pb_ds::test::direct_mask_range_hashing_t_<
	Allocator>,
	no_access_half_load_check_resize_trigger_policy,
	pb_ds::test::hash_exponential_size_policy_t_<
	Allocator> >::type
      performance_cc_policy4;

      typedef
      typename __gnu_cxx::typelist::create4<
	pb_ds::detail::false_type,
	pb_ds::test::direct_mask_range_hashing_t_<
	Allocator>,
	no_access_half_max_col_check_check_resize_trigger_policy,
	pb_ds::test::hash_exponential_size_policy_t_<
	Allocator> >::type
      performance_cc_policy5;

      typedef
      typename __gnu_cxx::typelist::create4<
	pb_ds::detail::false_type,
	pb_ds::test::direct_mask_range_hashing_t_<
	Allocator>,
	access_half_max_col_check_check_resize_trigger_policy,
	pb_ds::test::hash_exponential_size_policy_t_<
	Allocator> >::type
      regression_cc_policy0;

      typedef
      typename __gnu_cxx::typelist::create4<
	pb_ds::detail::false_type,
	pb_ds::test::direct_mask_range_hashing_t_<
	Allocator>,
	access_half_load_check_resize_trigger_policy,
	pb_ds::test::hash_exponential_size_policy_t_<
	Allocator> >::type
      regression_cc_policy1;

      typedef
      typename __gnu_cxx::typelist::create4<
	pb_ds::detail::true_type,
	pb_ds::test::direct_mod_range_hashing_t_<
	Allocator>,
	no_access_half_load_check_resize_trigger_policy,
	pb_ds::test::hash_prime_size_policy_t_ >::type
      regression_cc_policy2;

      typedef
      typename __gnu_cxx::typelist::create5<
	pb_ds::detail::false_type,
	lin_p_t,
	pb_ds::test::direct_mask_range_hashing_t_<
	Allocator>,
	no_access_half_load_check_resize_trigger_policy,
	pb_ds::test::hash_exponential_size_policy_t_<
	Allocator> >::type
      performance_gp_policy0;

      typedef
      typename __gnu_cxx::typelist::create5<
	pb_ds::detail::false_type,
	quad_p_t,
	pb_ds::test::direct_mod_range_hashing_t_<
	Allocator>,
	no_access_half_load_check_resize_trigger_policy,
	pb_ds::test::hash_prime_size_policy_t_ >::type
      performance_gp_policy1;

      typedef
      typename __gnu_cxx::typelist::create5<
	pb_ds::detail::false_type,
	quad_p_t,
	pb_ds::test::direct_mod_range_hashing_t_<
	Allocator>,
	access_half_load_check_resize_trigger_policy,
	pb_ds::test::hash_prime_size_policy_t_>::type
      regression_gp_policy0;

      typedef
      typename __gnu_cxx::typelist::create5<
	pb_ds::detail::true_type,
	lin_p_t,
	pb_ds::test::direct_mask_range_hashing_t_<
	Allocator>,
	access_half_load_check_resize_trigger_policy,
	pb_ds::test::hash_exponential_size_policy_t_<
	Allocator> >::type
      regression_gp_policy1;

      typedef
      typename __gnu_cxx::typelist::create6<
	performance_cc_policy0,
	performance_cc_policy1,
	performance_cc_policy2,
	performance_cc_policy3,
	performance_cc_policy4,
	performance_cc_policy5>::type
      performance_cc_range_hashing_policies;

      typedef
      typename __gnu_cxx::typelist::create3<
	regression_cc_policy0,
	regression_cc_policy1,
	regression_cc_policy2>::type
      regression_cc_range_hashing_policies;

      typedef
      typename __gnu_cxx::typelist::create2<
	performance_gp_policy0,
	performance_gp_policy1>::type
      performance_gp_range_hashing_policies;

      typedef
      typename __gnu_cxx::typelist::create2<
	regression_gp_policy0,
	regression_gp_policy1>::type
      regression_gp_range_hashing_policies;

      template<typename Policy_Tl>
      struct no_access_generic_cc_hash_table_t
      {
      private:
        typedef
	typename __gnu_cxx::typelist::at_index<
	Policy_Tl, 0>::type
	store_hash_indicator;

        enum
	  {
            store_hash = store_hash_indicator::value
	  };

        typedef
	typename __gnu_cxx::typelist::at_index<
	  Policy_Tl, 1>::type
	comb_hash_fn;

        typedef
	typename __gnu_cxx::typelist::at_index<
	  Policy_Tl, 2>::type
	trigger_policy;

        typedef
	typename __gnu_cxx::typelist::at_index<
	  Policy_Tl, 3>::type
	size_policy;

      public:
        typedef
	pb_ds::cc_hash_table<
	Key,
	Data,
	Hash_Fn,
	Eq_Fn,
	comb_hash_fn,
	pb_ds::hash_standard_resize_policy<
	size_policy,
	trigger_policy,
	false>,
	store_hash,
	Allocator>
	type;
      };

      template<typename Policy_Tl>
      struct access_generic_cc_hash_table_t
      {
      private:
        typedef
	typename __gnu_cxx::typelist::at_index<
	Policy_Tl, 0>::type
	store_hash_indicator;

        enum
	  {
            store_hash = store_hash_indicator::value
	  };

        typedef
	typename __gnu_cxx::typelist::at_index<
	  Policy_Tl, 1>::type
	comb_hash_fn;

        typedef
	typename __gnu_cxx::typelist::at_index<
	  Policy_Tl, 2>::type
	trigger_policy;

        typedef
	typename __gnu_cxx::typelist::at_index<
	  Policy_Tl, 3>::type
	size_policy;

      public:
        typedef
	pb_ds::cc_hash_table<
	Key,
	Data,
	Hash_Fn,
	Eq_Fn,
	comb_hash_fn,
	pb_ds::hash_standard_resize_policy<
	size_policy,
	trigger_policy,
	true>,
	store_hash,
	Allocator>
	type;
      };

      template<typename Policy_Tl>
      struct no_access_generic_gp_hash_table_t
      {
      private:
        typedef
	typename __gnu_cxx::typelist::at_index<
	Policy_Tl, 0>::type
	store_hash_indicator;

        enum
	  {
            store_hash = store_hash_indicator::value
	  };

        typedef
	typename __gnu_cxx::typelist::at_index<
	  Policy_Tl, 1>::type
	probe_fn;

        typedef
	typename __gnu_cxx::typelist::at_index<
	  Policy_Tl, 2>::type
	comb_probe_fn;

        typedef
	typename __gnu_cxx::typelist::at_index<
	  Policy_Tl, 3>::type
	trigger_policy;

        typedef
	typename __gnu_cxx::typelist::at_index<
	  Policy_Tl, 4>::type
	size_policy;

      public:
        typedef
	pb_ds::gp_hash_table<
	Key,
	Data,
	Hash_Fn,
	Eq_Fn,
	comb_probe_fn,
	probe_fn,
	pb_ds::hash_standard_resize_policy<
	size_policy,
	trigger_policy,
	false>,
	store_hash,
	Allocator>
	type;
      };

      template<typename Policy_Tl>
      struct access_generic_gp_hash_table_t
      {
      private:
        typedef
	typename __gnu_cxx::typelist::at_index<
	Policy_Tl, 0>::type
	store_hash_indicator;

        enum
	  {
            store_hash = store_hash_indicator::value
	  };

        typedef
	typename __gnu_cxx::typelist::at_index<
	  Policy_Tl, 1>::type
	probe_fn;

        typedef
	typename __gnu_cxx::typelist::at_index<
	  Policy_Tl, 2>::type
	comb_probe_fn;

        typedef
	typename __gnu_cxx::typelist::at_index<
	  Policy_Tl, 3>::type
	trigger_policy;

        typedef
	typename __gnu_cxx::typelist::at_index<
	  Policy_Tl, 4>::type
	size_policy;

      public:
        typedef
	pb_ds::gp_hash_table<
	Key,
	Data,
	Hash_Fn,
	Eq_Fn,
	comb_probe_fn,
	probe_fn,
	pb_ds::hash_standard_resize_policy<
	size_policy,
	trigger_policy,
	true>,
	store_hash,
	Allocator>
	type;
      };

      typedef
      typename __gnu_cxx::typelist::transform<
	performance_cc_range_hashing_policies,
	no_access_generic_cc_hash_table_t>::type
      performance_cc_types;

      typedef
      typename __gnu_cxx::typelist::transform<
	regression_cc_range_hashing_policies,
	access_generic_cc_hash_table_t>::type
      regression_cc_types;

      typedef
      typename __gnu_cxx::typelist::at_index<
	performance_cc_types,
	0>::type
      performance_min_cc_type;

      typedef
      typename __gnu_cxx::typelist::transform<
	performance_gp_range_hashing_policies,
	no_access_generic_gp_hash_table_t>::type
      performance_gp_types;

      typedef
      typename __gnu_cxx::typelist::transform<
	regression_gp_range_hashing_policies,
	access_generic_gp_hash_table_t>::type
      regression_gp_types;

      typedef
      typename __gnu_cxx::typelist::at_index<
	performance_gp_types,
	0>::type
      performance_min_gp_type;

    public:
      typedef
      typename __gnu_cxx::typelist::append<
      performance_cc_types,
      performance_gp_types>::type
      performance_tl;

      typedef
      typename __gnu_cxx::typelist::append<
	regression_gp_types,
	regression_cc_types>::type
      regression_tl;

      typedef
      typename __gnu_cxx::typelist::create1<
	performance_min_cc_type>::type
      performance_min_tl;
    };

    template<typename Key,
	     typename Data,
	     class Comb_Hash_Fn_TL,
	     class Comb_Probe_Fn_TL,
	     class Eq_Fn =
	     std::equal_to<Key>,
	     class Allocator =
	     std::allocator<
      std::pair<
      const Key,
      Data> > >
    struct ranged_hash_common_types
    {
    private:
      typedef typename Allocator::size_type size_type;

      typedef
      pb_ds::test::hash_load_check_resize_trigger_t_<
	Allocator,
	1, 8,
	1, 2,
	false>
      no_access_half_load_check_resize_trigger_policy;

      typedef
      pb_ds::test::hash_load_check_resize_trigger_t_<
	Allocator,
	1, 8,
	1, 1,
	false>
      no_access_one_load_check_resize_trigger_policy;

      typedef
      pb_ds::hash_standard_resize_policy<
	pb_ds::test::hash_exponential_size_policy_t_<
	Allocator>,
	no_access_half_load_check_resize_trigger_policy>
      mask_half_resize_policy_t;

      typedef
      pb_ds::hash_standard_resize_policy<
	pb_ds::test::hash_exponential_size_policy_t_<
	Allocator>,
	no_access_one_load_check_resize_trigger_policy>
      mask_one_resize_policy_t;

      typedef
      pb_ds::hash_standard_resize_policy<
	pb_ds::test::hash_prime_size_policy_t_,
	no_access_half_load_check_resize_trigger_policy>
      mod_half_resize_policy_t;

      typedef
      pb_ds::hash_standard_resize_policy<
	pb_ds::test::hash_prime_size_policy_t_,
	no_access_one_load_check_resize_trigger_policy>
      mod_one_resize_policy_t;

      template<typename Comb_Hash_Fn_>
      struct half_resize_policy_selector;

      template<typename Allocator_>
      struct half_resize_policy_selector<
        pb_ds::test::direct_mask_range_hashing_t_<
	Allocator_> >
      {
        typedef mask_half_resize_policy_t type;
      };

      template<typename Allocator_>
      struct half_resize_policy_selector<
        pb_ds::test::direct_mod_range_hashing_t_<
	Allocator_> >
      {
        typedef mod_half_resize_policy_t type;
      };

      template<typename Comb_Hash_Fn_>
      struct one_resize_policy_selector;

      template<typename Allocator_>
      struct one_resize_policy_selector<
        pb_ds::test::direct_mask_range_hashing_t_<
	Allocator_> >
      {
        typedef mask_one_resize_policy_t type;
      };

      template<typename Allocator_>
      struct one_resize_policy_selector<
        pb_ds::test::direct_mod_range_hashing_t_<
	Allocator_> >
      {
        typedef mod_one_resize_policy_t type;
      };

      template<typename Comb_Hash_Fn>
      struct generic_cc_hash_table_t
      {
        typedef
	pb_ds::cc_hash_table<
	  Key,
	  Data,
	  pb_ds::null_hash_fn,
	  Eq_Fn,
	  Comb_Hash_Fn,
	  typename one_resize_policy_selector<
	  typename Comb_Hash_Fn::comb_fn>::type,
	  false,
	  Allocator>
	type;
      };

      typedef
      typename __gnu_cxx::typelist::transform<
	Comb_Hash_Fn_TL,
	generic_cc_hash_table_t>::type
      performance_cc_types;

      template<typename Comb_Probe_Fn>
      struct no_access_generic_gp_hash_table_t
      {
        typedef
	pb_ds::gp_hash_table<
	  Key,
	  Data,
	  pb_ds::null_hash_fn,
	  Eq_Fn,
	  Comb_Probe_Fn,
	  pb_ds::null_probe_fn,
	  typename half_resize_policy_selector<
	  typename Comb_Probe_Fn::comb_fn>::type,
	  false,
	  Allocator>
	type;
      };

      typedef
      typename __gnu_cxx::typelist::transform<
	Comb_Probe_Fn_TL,
	no_access_generic_gp_hash_table_t>::type
      performance_gp_types;

    public:
      typedef
      typename __gnu_cxx::typelist::append<
      performance_cc_types,
      performance_gp_types>::type
      performance_tl;
    };

    template<typename Key, typename Data, class Eq_Fn = std::equal_to<Key>,
	     class Allocator =
	     std::allocator<char> >
    class lu_common_types
    {
    private:
      typedef typename Allocator::size_type size_type;

      typedef pb_ds::test::move_to_front_lu_policy_t_ mtf_u;

      typedef pb_ds::test::counter_lu_policy_t_<Allocator, 5> cnt_5_u;

      typedef typename __gnu_cxx::typelist::create1<mtf_u>::type lu_policy0;

      typedef typename __gnu_cxx::typelist::create1<cnt_5_u>::type lu_policy1;

      typedef
      typename __gnu_cxx::typelist::create2<
	lu_policy0,
	lu_policy1>::type
      lu_policies;

      template<typename Policy_Tl>
      struct generic_list_update_t
      {
      private:
        typedef
	typename __gnu_cxx::typelist::at_index<
	Policy_Tl, 0>::type
	update_policy_t;

      public:
        typedef
	pb_ds::list_update<
	Key,
	Data,
	Eq_Fn,
	update_policy_t,
	Allocator>
	type;
      };

      typedef
      typename __gnu_cxx::typelist::transform<
	lu_policies,
	generic_list_update_t>::type
      lu_types;

      typedef
      typename __gnu_cxx::typelist::at_index<
	lu_types,
	0>::type
      min_lu_type;

    public:
      typedef lu_types performance_tl;
      typedef lu_types regression_tl;

      typedef typename __gnu_cxx::typelist::create1<min_lu_type>::type performance_min_tl;
    };

    template<typename Key, typename Data, class Cmp_Fn = std::less<Key>,
	     template<typename Const_Node_Iterator,
		      class Node_Iterator,
		      class Cmp_Fn_,
		      class Allocator_>
    class Node_Update = pb_ds::null_tree_node_update,
	     class Allocator = std::allocator<std::pair<const Key, Data> > >
    struct tree_common_types
    {
    private:
      typedef
      pb_ds::tree<
      Key,
      Data,
      Cmp_Fn,
      pb_ds::ov_tree_tag,
      Node_Update,
      Allocator>
      ov_tree_assoc_container_t;

      typedef
      pb_ds::tree<
	Key,
	Data,
	Cmp_Fn,
	pb_ds::rb_tree_tag,
	Node_Update,
	Allocator>
      rb_tree_assoc_container_t;

      typedef
      pb_ds::tree<
	Key,
	Data,
	Cmp_Fn,
	pb_ds::splay_tree_tag,
	Node_Update,
	Allocator>
      splay_tree_assoc_container_t;

    public:
      typedef
      typename __gnu_cxx::typelist::create3<
      splay_tree_assoc_container_t,
      rb_tree_assoc_container_t,
      ov_tree_assoc_container_t>::type
      performance_tl;

      typedef
      typename __gnu_cxx::typelist::create3<
	ov_tree_assoc_container_t,
	splay_tree_assoc_container_t,
	rb_tree_assoc_container_t>::type
      regression_tl;

      typedef
      typename __gnu_cxx::typelist::create1<
	rb_tree_assoc_container_t>::type
      performance_min_tl;
    };

    template<typename Key,
	     typename Data,
	     class E_Access_Traits =
	     typename pb_ds::detail::default_trie_e_access_traits<Key>::type,
	     class Tag = pb_ds::pat_trie_tag,
	     template<typename Const_Node_Iterator,
		      typename Node_Iterator,
		      class E_Access_Traits_,
		      typename Allocator_>
    class Node_Update = pb_ds::null_trie_node_update,
	     class Allocator = std::allocator<char> >
    class trie_common_types
    {
    private:
      typedef pb_ds::trie<Key, Data, E_Access_Traits, Tag, Node_Update, Allocator> type;

    public:
      typedef typename __gnu_cxx::typelist::create1<type>::type performance_tl;
      typedef typename __gnu_cxx::typelist::create1<type>::type regression_tl;
      typedef typename __gnu_cxx::typelist::create1<type>::type performance_min_tl;
    };

  } // namespace test

} // namespace pb_ds

#endif // #ifndef PB_DS_COMMON_TYPES_HPP
