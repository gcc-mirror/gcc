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
 * @file container_base_dispatch.hpp
 * Contains an associative container dispatching base.
 */

#ifndef PB_DS_ASSOC_CNTNR_BASE_DS_DISPATCHER_HPP
#define PB_DS_ASSOC_CNTNR_BASE_DS_DISPATCHER_HPP

#include <ext/pb_ds/detail/typelist.hpp>

#define PB_DS_DATA_TRUE_INDICATOR
#include <ext/pb_ds/detail/list_update_map_/lu_map_.hpp>
#undef PB_DS_DATA_TRUE_INDICATOR

#define PB_DS_DATA_FALSE_INDICATOR
#include <ext/pb_ds/detail/list_update_map_/lu_map_.hpp>
#undef PB_DS_DATA_FALSE_INDICATOR

#define PB_DS_DATA_TRUE_INDICATOR
#include <ext/pb_ds/detail/rb_tree_map_/rb_tree_.hpp>
#undef PB_DS_DATA_TRUE_INDICATOR

#define PB_DS_DATA_FALSE_INDICATOR
#include <ext/pb_ds/detail/rb_tree_map_/rb_tree_.hpp>
#undef PB_DS_DATA_FALSE_INDICATOR

#define PB_DS_DATA_TRUE_INDICATOR
#include <ext/pb_ds/detail/splay_tree_/splay_tree_.hpp>
#undef PB_DS_DATA_TRUE_INDICATOR

#define PB_DS_DATA_FALSE_INDICATOR
#include <ext/pb_ds/detail/splay_tree_/splay_tree_.hpp>
#undef PB_DS_DATA_FALSE_INDICATOR

#define PB_DS_DATA_TRUE_INDICATOR
#include <ext/pb_ds/detail/ov_tree_map_/ov_tree_map_.hpp>
#undef PB_DS_DATA_TRUE_INDICATOR

#define PB_DS_DATA_FALSE_INDICATOR
#include <ext/pb_ds/detail/ov_tree_map_/ov_tree_map_.hpp>
#undef PB_DS_DATA_FALSE_INDICATOR

#define PB_DS_DATA_TRUE_INDICATOR
#include <ext/pb_ds/detail/cc_hash_table_map_/cc_ht_map_.hpp>
#undef PB_DS_DATA_TRUE_INDICATOR

#define PB_DS_DATA_FALSE_INDICATOR
#include <ext/pb_ds/detail/cc_hash_table_map_/cc_ht_map_.hpp>
#undef PB_DS_DATA_FALSE_INDICATOR

#define PB_DS_DATA_TRUE_INDICATOR
#include <ext/pb_ds/detail/gp_hash_table_map_/gp_ht_map_.hpp>
#undef PB_DS_DATA_TRUE_INDICATOR

#define PB_DS_DATA_FALSE_INDICATOR
#include <ext/pb_ds/detail/gp_hash_table_map_/gp_ht_map_.hpp>
#undef PB_DS_DATA_FALSE_INDICATOR

#define PB_DS_DATA_TRUE_INDICATOR
#include <ext/pb_ds/detail/pat_trie_/pat_trie_.hpp>
#undef PB_DS_DATA_TRUE_INDICATOR

#define PB_DS_DATA_FALSE_INDICATOR
#include <ext/pb_ds/detail/pat_trie_/pat_trie_.hpp>
#undef PB_DS_DATA_FALSE_INDICATOR

namespace pb_ds
{
  namespace detail
    {

      template<typename Key,
	       typename Mapped,
	       class Data_Structure_Taq,
	       class Policy_Tl,
	       class Allocator>
      struct container_base_dispatch;

      template<typename Key,
	       typename Mapped,
	       class Policy_Tl,
	       class Allocator>
      struct container_base_dispatch<
	Key,
	Mapped,
	list_update_tag,
	Policy_Tl,
	Allocator>
      {
	typedef
        lu_map_data_<
	  Key,
	  Mapped,
	  typename typelist_at_index<Policy_Tl, 0>::type,
	  Allocator,
	  typename typelist_at_index<Policy_Tl, 1>::type>
        type;
      };

      template<typename Key, class Policy_Tl, class Allocator>
      struct container_base_dispatch<
	Key,
	null_mapped_type,
	list_update_tag,
	Policy_Tl,
	Allocator>
      {
	typedef
        lu_map_no_data_<
	  Key,
	  null_mapped_type,
	  typename typelist_at_index<Policy_Tl, 0>::type,
	  Allocator,
	  typename typelist_at_index<Policy_Tl, 1>::type>
        type;
      };

      template<typename Key,
	       typename Mapped,
	       class Policy_Tl,
	       class Allocator>
      struct container_base_dispatch<
	Key,
	Mapped,
	pat_trie_tag,
	Policy_Tl,
	Allocator>
      {
	typedef
        pat_trie_data_<
	  Key,
	  Mapped,
	  typename typelist_at_index<Policy_Tl, 1>::type,
	  Allocator>
        type;
      };

      template<typename Key, class Policy_Tl, class Allocator>
      struct container_base_dispatch<
	Key,
	null_mapped_type,
	pat_trie_tag,
	Policy_Tl,
	Allocator>
      {
	typedef
        pat_trie_no_data_<
	  Key,
	  null_mapped_type,
	  typename typelist_at_index<Policy_Tl, 1>::type,
	  Allocator>
        type;
      };

      template<typename Key,
	       typename Mapped,
	       class Policy_Tl,
	       class Allocator>
      struct container_base_dispatch<
	Key,
	Mapped,
	rb_tree_tag,
	Policy_Tl,
	Allocator>
      {
	typedef
        rb_tree_data_<
	  Key,
	  Mapped,
	  typename typelist_at_index<Policy_Tl, 0>::type,
	  typename typelist_at_index<Policy_Tl, 1>::type,
	  Allocator>
        type;
      };

      template<typename Key, class Policy_Tl, class Allocator>
      struct container_base_dispatch<
	Key,
	null_mapped_type,
	rb_tree_tag,
	Policy_Tl,
	Allocator>
      {
	typedef
        rb_tree_no_data_<
	  Key,
	  null_mapped_type,
	  typename typelist_at_index<Policy_Tl, 0>::type,
	  typename typelist_at_index<Policy_Tl, 1>::type,
	  Allocator>
        type;
      };

      template<typename Key,
	       typename Mapped,
	       class Policy_Tl,
	       class Allocator>
      struct container_base_dispatch<
	Key,
	Mapped,
	splay_tree_tag,
	Policy_Tl,
	Allocator>

      {
	typedef
        splay_tree_data_<
	  Key,
	  Mapped,
	  typename typelist_at_index<Policy_Tl, 0>::type,
	  typename typelist_at_index<Policy_Tl, 1>::type,
	  Allocator>
        type;
      };

      template<typename Key, class Policy_Tl, class Allocator>
      struct container_base_dispatch<
	Key,
	null_mapped_type,
	splay_tree_tag,
	Policy_Tl,
	Allocator>
      {
	typedef
        splay_tree_no_data_<
	  Key,
	  null_mapped_type,
	  typename typelist_at_index<Policy_Tl, 0>::type,
	  typename typelist_at_index<Policy_Tl, 1>::type,
	  Allocator>
        type;
      };

      template<typename Key,
	       typename Mapped,
	       class Policy_Tl,
	       class Allocator>
      struct container_base_dispatch<
	Key,
	Mapped,
	ov_tree_tag,
	Policy_Tl,
	Allocator>
      {
	typedef
        ov_tree_data_<
	  Key,
	  Mapped,
	  typename typelist_at_index<Policy_Tl, 0>::type,
	  typename typelist_at_index<Policy_Tl, 1>::type,
	  Allocator>
        type;
      };

      template<typename Key, class Policy_Tl, class Allocator>
      struct container_base_dispatch<
	Key,
	null_mapped_type,
	ov_tree_tag,
	Policy_Tl,
	Allocator>
      {
	typedef
        ov_tree_no_data_<
	  Key,
	  null_mapped_type,
	  typename typelist_at_index<Policy_Tl, 0>::type,
	  typename typelist_at_index<Policy_Tl, 1>::type,
	  Allocator>
        type;
      };

      template<typename Key,
	       typename Mapped,
	       class Policy_Tl,
	       class Allocator>
      struct container_base_dispatch<
	Key,
	Mapped,
	cc_hash_tag,
	Policy_Tl,
	Allocator>
      {
	typedef
        cc_ht_map_data_<
	  Key,
	  Mapped,
	  typename typelist_at_index<Policy_Tl, 0>::type,
	  typename typelist_at_index<Policy_Tl, 1>::type,
	  Allocator,
	  typelist_at_index<Policy_Tl, 3>::type::value,
	  typename typelist_at_index<Policy_Tl, 4>::type,
	  typename typelist_at_index<Policy_Tl, 2>::type>
        type;
      };

      template<typename Key, class Policy_Tl, class Allocator>
      struct container_base_dispatch<
	Key,
	null_mapped_type,
	cc_hash_tag,
	Policy_Tl,
	Allocator>
      {
	typedef
        cc_ht_map_no_data_<
	  Key,
	  null_mapped_type,
	  typename typelist_at_index<Policy_Tl, 0>::type,
	  typename typelist_at_index<Policy_Tl, 1>::type,
	  Allocator,
	  typelist_at_index<Policy_Tl, 3>::type::value,
	  typename typelist_at_index<Policy_Tl, 4>::type,
	  typename typelist_at_index<Policy_Tl, 2>::type>
        type;
      };

      template<typename Key,
	       typename Mapped,
	       class Policy_Tl,
	       class Allocator>
      struct container_base_dispatch<
	Key,
	Mapped,
	gp_hash_tag,
	Policy_Tl,
	Allocator>
      {
	typedef
        gp_ht_map_data_<
	  Key,
	  Mapped,
	  typename typelist_at_index<Policy_Tl, 0>::type,
	  typename typelist_at_index<Policy_Tl, 1>::type,
	  Allocator,
	  typelist_at_index<Policy_Tl, 3>::type::value,
	  typename typelist_at_index<Policy_Tl, 4>::type,
	  typename typelist_at_index<Policy_Tl, 5>::type,
	  typename typelist_at_index<Policy_Tl, 2>::type>
        type;
      };

      template<typename Key, class Policy_Tl, class Allocator>
      struct container_base_dispatch<
	Key,
	null_mapped_type,
	gp_hash_tag,
	Policy_Tl,
	Allocator>
      {
	typedef
        gp_ht_map_no_data_<
	  Key,
	  null_mapped_type,
	  typename typelist_at_index<Policy_Tl, 0>::type,
	  typename typelist_at_index<Policy_Tl, 1>::type,
	  Allocator,
	  typelist_at_index<Policy_Tl, 3>::type::value,
	  typename typelist_at_index<Policy_Tl, 4>::type,
	  typename typelist_at_index<Policy_Tl, 5>::type,
	  typename typelist_at_index<Policy_Tl, 2>::type>
        type;
      };

    } // namespace detail
} // namespace pb_ds

#endif // #ifndef PB_DS_ASSOC_CNTNR_BASE_DS_DISPATCHER_HPP
