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
 * @file ds_trait_imp.hpp
 * Contains data-structure traits implementation.
 */

#ifndef DS_TRAIT_IMP_HPP
#define DS_TRAIT_IMP_HPP

namespace detail
{

  template<class Cntnr, class DS_Category>
  struct erase_can_throw_imp
  {
    enum
      {
	value =
	pb_assoc::detail::is_same_type<
	DS_Category,
	ov_tree_ds_tag>::value
      };
  };

  template<class Cntnr>
  struct erase_can_throw_imp<
    Cntnr,
    compound_ds_tag>
  {
    enum
      {
	value = Cntnr::erase_can_throw
      };
  };

  template<class Cntnr, class DS_Category>
  struct erase_iterators_imp
  {
    enum
      {
	value =
	pb_assoc::detail::is_same_type<
	DS_Category,
	rb_tree_ds_tag>::value ||
	pb_assoc::detail::is_same_type<
	DS_Category,
	splay_tree_ds_tag>::value
      };
  };

  template<class Cntnr>
  struct erase_iterators_imp<
    Cntnr,
    compound_ds_tag>
  {
    enum
      {
	value = Cntnr::erase_iterators
      };
  };

  template<class Cntnr, class DS_Category>
  struct order_preserving_imp
  {
    enum
      {
	value =
	pb_assoc::detail::is_same_type<
	DS_Category,
	rb_tree_ds_tag>::value ||
	pb_assoc::detail::is_same_type<
	DS_Category,
	splay_tree_ds_tag>::value ||
	pb_assoc::detail::is_same_type<
	DS_Category,
	ov_tree_ds_tag>::value
      };
  };

  template<class Cntnr>
  struct order_preserving_imp<
    Cntnr,
    compound_ds_tag>
  {
    enum
      {
	value = Cntnr::order_preserving
      };
  };

  template<class Cntnr, class DS_Category>
  struct invalidation_guarantee_imp
  {
  private:
    enum
      {
	node_based =
	pb_assoc::detail::is_same_type<
	DS_Category,
	cc_hash_ds_tag>::value ||
	pb_assoc::detail::is_same_type<
	DS_Category,
	rb_tree_ds_tag>::value ||
	pb_assoc::detail::is_same_type<
	DS_Category,
	splay_tree_ds_tag>::value ||
	pb_assoc::detail::is_same_type<
	DS_Category,
	lu_ds_tag>::value
      };

    enum
      {
	vector_organized =
	pb_assoc::detail::is_same_type<
	DS_Category,
	cc_hash_ds_tag>::value
      };

  public:
    typedef
    typename cond_type<
      node_based,
      typename cond_type<
      vector_organized,
      find_invalidation_guarantee,
      range_invalidation_guarantee>::type,
      basic_invalidation_guarantee>::type
    type;
  };

  template<class Cntnr>
  struct invalidation_guarantee_imp<
    Cntnr,
    compound_ds_tag>
  {
    typedef typename Cntnr::invalidation_guarantee type;
  };

  template<class Cntnr, class DS_Category>
  struct reverse_iteration_imp
  {
    enum
      {
	value =
	is_same_type<
	DS_Category,
	rb_tree_ds_tag>::value ||
	is_same_type<
	DS_Category,
	splay_tree_ds_tag>::value
      };
  };

  template<class Cntnr>
  struct reverse_iteration_imp<
    Cntnr,
    compound_ds_tag>
  {
    enum
      {
	value = Cntnr::reverse_iteration
      };
  };

  template<class Cntnr, class DS_Category>
  struct split_join_imp
  {
    enum
      {
	value =
	is_same_type<
	DS_Category,
	rb_tree_ds_tag>::value ||
	is_same_type<
	DS_Category,
	splay_tree_ds_tag>::value ||
	is_same_type<
	DS_Category,
	ov_tree_ds_tag>::value
      };
  };

  template<class Cntnr>
  struct split_join_imp<
    Cntnr,
    compound_ds_tag>
  {
    enum
      {
	value = false
      };
  };

  template<class Cntnr>
  struct basic_data_structure_traits
  {

  private:
    typedef Cntnr cntnr;

    typedef typename Cntnr::ds_category ds_category;

  public:
    enum
      {
	erase_can_throw =
	pb_assoc::detail::erase_can_throw_imp<
	cntnr,
	ds_category>::value
      };

    enum
      {
	order_preserving =
	pb_assoc::detail::order_preserving_imp<
	cntnr,
	ds_category>::value
      };

    enum
      {
	erase_iterators =
	pb_assoc::detail::erase_iterators_imp<
	Cntnr,
	ds_category>::value
      };

    typedef
    typename pb_assoc::detail::invalidation_guarantee_imp<
      cntnr,
      ds_category>::type
    invalidation_guarantee;

    enum
      {
	reverse_iteration =
	pb_assoc::detail::reverse_iteration_imp<
	cntnr,
	ds_category>::value
      };

    enum
      {
	split_join =
	pb_assoc::detail::split_join_imp<
	cntnr,
	ds_category>::value
      };
  };

  template<class Cntnr, class DS_Category>
  struct data_structure_traits : public basic_data_structure_traits<
    Cntnr>
  { };

  template<class Cntnr>
  struct data_structure_traits<
    Cntnr,
    rb_tree_ds_tag> : public basic_data_structure_traits<
    Cntnr>
  {
  public:
    enum
      {
	split_join_can_throw = false
      };
  };

  template<class Cntnr>
  struct data_structure_traits<
    Cntnr,
    splay_tree_ds_tag> : public basic_data_structure_traits<
    Cntnr>
  {
  public:
    enum
      {
	split_join_can_throw = false
      };
  };

  template<class Cntnr>
  struct data_structure_traits<
    Cntnr,
    ov_tree_ds_tag> : public basic_data_structure_traits<
    Cntnr>
  {
  public:
    enum
      {
	split_join_can_throw = true
      };
  };

} // namespace detail

#endif // #ifndef DS_TRAIT_IMP_HPP
