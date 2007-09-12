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
 * @file defs_fn_imps.hpp
 * Containsert a random regression test for a specific container type.
 */

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
defs()
{
  // General container types.

  typedef typename Cntnr::size_type test_size_type;

  typedef typename Cntnr::difference_type difference_type;

  key_defs();

  mapped_defs();

  value_defs();

  iterator_defs();

  node_iterator_defs(__gnu_pbds::detail::integral_constant<int,
		     container_traits::order_preserving>());

  policy_defs();
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
key_defs()
{
  typedef typename Cntnr::key_type test_key_type;

  typedef typename Cntnr::key_reference test_key_reference;

  typedef typename Cntnr::const_key_reference test_const_key_reference;

  typedef typename Cntnr::key_pointer test_key_pointer;

  typedef typename Cntnr::const_key_pointer test_const_key_pointer;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
mapped_defs()
{
  typedef typename Cntnr::mapped_type test_mapped_type;

  typedef typename Cntnr::mapped_reference test_mapped_reference;

  typedef
    typename Cntnr::const_mapped_reference
    test_const_mapped_reference;

  typedef typename Cntnr::mapped_pointer test_mapped_pointer;

  typedef typename Cntnr::const_mapped_pointer test_const_mapped_pointer;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
value_defs()
{
  typedef typename Cntnr::value_type test_value_type;

  typedef typename Cntnr::reference test_reference;

  typedef typename Cntnr::const_reference test_const_reference;

  typedef typename Cntnr::pointer test_pointer;

  typedef typename Cntnr::const_pointer test_const_pointer;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
ds_defs()
{
  typedef __gnu_pbds::container_traits< Cntnr> test_container_traits;

  typedef typename test_container_traits::container_category test_container_category;

  typedef
    typename test_container_traits::invalidation_guarantee
    test_invalidation_guarantee;

  enum
    {
      test_order_preserving =
      test_container_traits::order_preserving
    };

  enum
    {
      test_erase_can_throw =
      test_container_traits::erase_can_throw
    };
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
iterator_defs()
{
  typedef typename Cntnr::point_iterator test_point_iterator;

  typedef typename Cntnr::const_point_iterator const_test_point_iterator;

  typedef typename Cntnr::iterator test_iterator;

  typedef typename Cntnr::const_iterator const_test_iterator;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
node_iterator_defs(__gnu_pbds::detail::false_type)
{ }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
node_iterator_defs(__gnu_pbds::detail::true_type)
{
  typedef typename Cntnr::node_iterator test_node_iterator;

  typedef typename Cntnr::const_node_iterator test_const_node_iterator;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_defs()
{
  typedef typename Cntnr::allocator test_allocator;

  policy_defs(typename Cntnr::container_category());
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_defs(__gnu_pbds::basic_hash_tag)
{
  typedef typename Cntnr::hash_fn test_hash_fn;

  typedef typename Cntnr::eq_fn test_eq_fn;

  typedef typename Cntnr::resize_policy test_resize_policy;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_defs(__gnu_pbds::cc_hash_tag)
{
  policy_defs(__gnu_pbds::basic_hash_tag());

  typedef typename Cntnr::comb_hash_fn test_comb_hash_fn;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_defs(__gnu_pbds::gp_hash_tag)
{
  policy_defs(__gnu_pbds::basic_hash_tag());

  typedef typename Cntnr::comb_probe_fn test_comb_probe_fn;

  typedef typename Cntnr::probe_fn test_probe_fn;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_defs(__gnu_pbds::tree_tag)
{
  typedef typename Cntnr::cmp_fn test_cmp_fn;

  typedef typename Cntnr::node_update test_node_update;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_defs(__gnu_pbds::list_update_tag)
{
  typedef typename Cntnr::eq_fn test_eq_fn;

  typedef typename Cntnr::update_policy test_update_policy;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_defs(__gnu_pbds::pat_trie_tag)
{
  typedef typename Cntnr::e_access_traits e_access_traits;
}
