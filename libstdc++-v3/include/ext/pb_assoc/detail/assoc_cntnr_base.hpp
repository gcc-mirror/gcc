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
 * @file assoc_cntnr_base.hpp
 * Contains an associative container dispatching base.
 */

#ifndef ASSOC_CNTNR_BASE_HPP
#define ASSOC_CNTNR_BASE_HPP

#include <ext/pb_assoc/detail/typelist.hpp>

#define PB_ASSOC_DATA_TRUE_INDICATOR
#include <ext/pb_assoc/detail/lu_map_/lu_map_.hpp>
#undef PB_ASSOC_DATA_TRUE_INDICATOR

#define PB_ASSOC_DATA_FALSE_INDICATOR
#include <ext/pb_assoc/detail/lu_map_/lu_map_.hpp>
#undef PB_ASSOC_DATA_FALSE_INDICATOR

#define PB_ASSOC_DATA_TRUE_INDICATOR
#include <ext/pb_assoc/detail/rb_tree_map_/rb_tree_.hpp>
#undef PB_ASSOC_DATA_TRUE_INDICATOR

#define PB_ASSOC_DATA_FALSE_INDICATOR
#include <ext/pb_assoc/detail/rb_tree_map_/rb_tree_.hpp>
#undef PB_ASSOC_DATA_FALSE_INDICATOR

#define PB_ASSOC_DATA_TRUE_INDICATOR
#include <ext/pb_assoc/detail/splay_tree_/splay_tree_.hpp>
#undef PB_ASSOC_DATA_TRUE_INDICATOR

#define PB_ASSOC_DATA_FALSE_INDICATOR
#include <ext/pb_assoc/detail/splay_tree_/splay_tree_.hpp>
#undef PB_ASSOC_DATA_FALSE_INDICATOR

#define PB_ASSOC_DATA_TRUE_INDICATOR
#include <ext/pb_assoc/detail/ov_tree_map_/ov_tree_map_.hpp>
#undef PB_ASSOC_DATA_TRUE_INDICATOR

#define PB_ASSOC_DATA_FALSE_INDICATOR
#include <ext/pb_assoc/detail/ov_tree_map_/ov_tree_map_.hpp>
#undef PB_ASSOC_DATA_FALSE_INDICATOR

#define PB_ASSOC_DATA_TRUE_INDICATOR
#include <ext/pb_assoc/detail/cc_ht_map_/cc_ht_map_.hpp>
#undef PB_ASSOC_DATA_TRUE_INDICATOR

#define PB_ASSOC_DATA_FALSE_INDICATOR
#include <ext/pb_assoc/detail/cc_ht_map_/cc_ht_map_.hpp>
#undef PB_ASSOC_DATA_FALSE_INDICATOR

#define PB_ASSOC_DATA_TRUE_INDICATOR
#include <ext/pb_assoc/detail/gp_ht_map_/gp_ht_map_.hpp>
#undef PB_ASSOC_DATA_TRUE_INDICATOR

#define PB_ASSOC_DATA_FALSE_INDICATOR
#include <ext/pb_assoc/detail/gp_ht_map_/gp_ht_map_.hpp>
#undef PB_ASSOC_DATA_FALSE_INDICATOR

namespace pb_assoc
{

  namespace detail
  {

    template<typename Key,
	     typename Data,
	     class Data_Structure_Taq,
	     class Policy_Tl,
	     class Allocator>
    struct assoc_cntnr_base;

    template<typename Key, typename Data, class Policy_Tl, class Allocator>
    struct assoc_cntnr_base<
      Key,
      Data,
      lu_ds_tag,
      Policy_Tl,
      Allocator>
    {
      typedef
      lu_map_data_<
	Key,
	Data,
	typename typelist_at_index<Policy_Tl, 0>::type,
	Allocator,
	typename typelist_at_index<Policy_Tl, 1>::type>
      type;
    };

    template<typename Key, class Policy_Tl, class Allocator>
    struct assoc_cntnr_base<
      Key,
      null_data_type,
      lu_ds_tag,
      Policy_Tl,
      Allocator>
    {
      typedef
      lu_map_no_data_<
	Key,
	null_data_type,
	typename typelist_at_index<Policy_Tl, 0>::type,
	Allocator,
	typename typelist_at_index<Policy_Tl, 1>::type>
      type;
    };

    template<typename Key, typename Data, class Policy_Tl, class Allocator>
    struct assoc_cntnr_base<
      Key,
      Data,
      rb_tree_ds_tag,
      Policy_Tl,
      Allocator>
    {
      typedef
      rb_tree_data_<
	Key,
	Data,
	typename typelist_at_index<Policy_Tl, 0>::type,
	Allocator,
	typename typelist_at_index<Policy_Tl, 1>::type>
      type;
    };

    template<typename Key, class Policy_Tl, class Allocator>
    struct assoc_cntnr_base<
      Key,
      null_data_type,
      rb_tree_ds_tag,
      Policy_Tl,
      Allocator>
    {
      typedef
      rb_tree_no_data_<
	Key,
	null_data_type,
	typename typelist_at_index<Policy_Tl, 0>::type,
	Allocator,
	typename typelist_at_index<Policy_Tl, 1>::type>
      type;
    };

    template<typename Key, typename Data, class Policy_Tl, class Allocator>
    struct assoc_cntnr_base<
      Key,
      Data,
      splay_tree_ds_tag,
      Policy_Tl,
      Allocator>

    {
      typedef
      splay_tree_data_<
	Key,
	Data,
	typename typelist_at_index<Policy_Tl, 0>::type,
	Allocator,
	typename typelist_at_index<Policy_Tl, 1>::type>
      type;
    };

    template<typename Key, class Policy_Tl, class Allocator>
    struct assoc_cntnr_base<
      Key,
      null_data_type,
      splay_tree_ds_tag,
      Policy_Tl,
      Allocator>
    {
      typedef
      splay_tree_no_data_<
	Key,
	null_data_type,
	typename typelist_at_index<Policy_Tl, 0>::type,
	Allocator,
	typename typelist_at_index<Policy_Tl, 1>::type>
      type;
    };

    template<typename Key, typename Data, class Policy_Tl, class Allocator>
    struct assoc_cntnr_base<
      Key,
      Data,
      ov_tree_ds_tag,
      Policy_Tl,
      Allocator>
    {
      typedef
      ov_tree_data_<
	Key,
	Data,
	typename typelist_at_index<Policy_Tl, 0>::type,
	Allocator,
	typename typelist_at_index<Policy_Tl, 1>::type>
      type;
    };

    template<typename Key, class Policy_Tl, class Allocator>
    struct assoc_cntnr_base<
      Key,
      null_data_type,
      ov_tree_ds_tag,
      Policy_Tl,
      Allocator>
    {
      typedef
      ov_tree_no_data_<
	Key,
	null_data_type,
	typename typelist_at_index<Policy_Tl, 0>::type,
	Allocator,
	typename typelist_at_index<Policy_Tl, 1>::type>
      type;
    };

    template<typename Key, typename Data, class Policy_Tl, class Allocator>
    struct assoc_cntnr_base<
      Key,
      Data,
      cc_hash_ds_tag,
      Policy_Tl,
      Allocator>
    {
      typedef
      cc_ht_map_data_<
	Key,
	Data,
	typename typelist_at_index<Policy_Tl, 0>::type,
	typename typelist_at_index<Policy_Tl, 1>::type,
	Allocator,
	typelist_at_index<Policy_Tl, 3>::type::value,
	typename typelist_at_index<Policy_Tl, 4>::type,
	typename typelist_at_index<Policy_Tl, 2>::type>
      type;
    };

    template<typename Key, class Policy_Tl, class Allocator>
    struct assoc_cntnr_base<
      Key,
      null_data_type,
      cc_hash_ds_tag,
      Policy_Tl,
      Allocator>
    {
      typedef
      cc_ht_map_no_data_<
	Key,
	null_data_type,
	typename typelist_at_index<Policy_Tl, 0>::type,
	typename typelist_at_index<Policy_Tl, 1>::type,
	Allocator,
	typelist_at_index<Policy_Tl, 3>::type::value,
	typename typelist_at_index<Policy_Tl, 4>::type,
	typename typelist_at_index<Policy_Tl, 2>::type>
      type;
    };

    template<typename Key, typename Data, class Policy_Tl, class Allocator>
    struct assoc_cntnr_base<
      Key,
      Data,
      gp_hash_ds_tag,
      Policy_Tl,
      Allocator>
    {
      typedef
      gp_ht_map_data_<
	Key,
	Data,
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
    struct assoc_cntnr_base<
      Key,
      null_data_type,
      gp_hash_ds_tag,
      Policy_Tl,
      Allocator>
    {
      typedef
      gp_ht_map_no_data_<
	Key,
	null_data_type,
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

} // namespace pb_assoc

#endif // #ifndef ASSOC_CNTNR_BASE_HPP
