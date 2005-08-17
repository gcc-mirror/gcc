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
 * @file ds_trait.hpp
 * Contains data-structure traits.
 */

#ifndef DS_TRAIT_HPP
#define DS_TRAIT_HPP

#include <ext/pb_assoc/detail/type_utils.hpp>

namespace pb_assoc
{
  struct basic_invalidation_guarantee
  { };

  struct find_invalidation_guarantee : public basic_invalidation_guarantee
  { };

  struct range_invalidation_guarantee : public find_invalidation_guarantee
  { };

  struct basic_ds_tag
  { };

  struct basic_hash_ds_tag : public basic_ds_tag
  { };

  struct cc_hash_ds_tag : public basic_hash_ds_tag
  { };

  struct gp_hash_ds_tag : public basic_hash_ds_tag
  { };

  struct basic_tree_ds_tag : public basic_ds_tag
  { };

  struct rb_tree_ds_tag : public basic_tree_ds_tag
  { };

  struct splay_tree_ds_tag : public basic_tree_ds_tag
  { };

  struct ov_tree_ds_tag : public basic_tree_ds_tag
  { };

  struct lu_ds_tag : public basic_ds_tag
  { };

  struct compound_ds_tag : public basic_ds_tag
  { };

#include <ext/pb_assoc/detail/ds_trait_imp.hpp>

#define PB_ASSOC_BASE_C_DEC \
	detail::data_structure_traits<Cntnr, typename Cntnr::ds_category>

  template<typename Cntnr>
    struct ds_traits 
    : private detail::data_structure_traits<Cntnr, typename Cntnr::ds_category>
    {
    public:
      enum
	{
	  erase_can_throw = PB_ASSOC_BASE_C_DEC::erase_can_throw,
	  order_preserving = PB_ASSOC_BASE_C_DEC::order_preserving,
	  erase_iterators = PB_ASSOC_BASE_C_DEC::erase_iterators,
	  reverse_iteration = PB_ASSOC_BASE_C_DEC::reverse_iteration,
	  split_join = PB_ASSOC_BASE_C_DEC::split_join
	};
      
      typedef typename PB_ASSOC_BASE_C_DEC::invalidation_guarantee 
      invalidation_guarantee;

    /*
      enum
      {
      split_join_can_throw = PB_ASSOC_BASE_C_DEC::split_join_can_throw
      };
    */
  };

#undef PB_ASSOC_BASE_C_DEC

} // namespace pb_assoc

#endif // #ifndef DS_TRAIT_HPP
