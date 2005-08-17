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
 * @file standard_policies.hpp
 * Contains standard policies for containers.
 */

#ifndef STANDARD_POLICIES_HPP
#define STANDARD_POLICIES_HPP

#include <memory>
#include <ext/pb_assoc/hash_policy.hpp>
#include <ext/pb_assoc/lu_policy.hpp>
#include <ext/pb_assoc/tree_policy.hpp>
#if defined(__GNUC__)
#include <ext/hash_map>
#elif defined(_MSC_VER)
#include <hash_map>
#else
#error "Unable to determine the namespaces for your compiler. Please" \
	"Contact pbassoc@gmail.com"
#endif // #if defined(__GNUC__)

namespace pb_assoc
{

  namespace detail
  {

#ifdef __GNUC__

#define PB_ASSOC_HASH_NAMESPACE \
	__gnu_cxx

    template<typename Key>
    struct def_hash_fn
    {
      typedef PB_ASSOC_HASH_NAMESPACE::hash<Key> type;
    };

    template<typename Key>
    struct def_eq_fn
    {
      typedef PB_ASSOC_HASH_NAMESPACE::equal_to<Key> type;
    };

#elif defined(_MSC_VER)

    template<typename Key>
    struct hash_value_class_adapter
    {
      inline size_t
      operator()(const Key& r_key) const
      {
	return (stdext::hash_value(r_key));
      }
    };

    template<typename Key>
    struct def_hash_fn
    {
      typedef hash_value_class_adapter<Key> type;
    };

    template<typename Key>
    struct def_eq_fn
    {
      typedef std::equal_to<Key> type;
    };

#else // #elif defined(_MSC_VER)

#error Sorry, cannot determine headers, namespaces, etc. for your compiler.
#error If you encounter this error, pls write to pbassoc@gmail.com

#endif // #elif defined(_MSC_VER)

    enum
      {
	def_store_hash = false
      };

    struct def_comb_hash_fn
    {
      typedef pb_assoc::direct_mask_range_hashing<> type;
    };

    template<class Comb_Hash_Fn>
    struct def_resize_policy
    {
    private:
      typedef typename Comb_Hash_Fn::size_type size_type;

      typedef
      typename pb_assoc::detail::cond_type<
	pb_assoc::detail::is_same_type<
	pb_assoc::direct_mask_range_hashing<size_type>,
	Comb_Hash_Fn>::value,
	pb_assoc::hash_exponential_size_policy<
	size_type>,
	pb_assoc::hash_prime_size_policy>::type
      size_policy_type;

    public:
      typedef
      pb_assoc::hash_standard_resize_policy<
	size_policy_type,
	pb_assoc::hash_load_check_resize_trigger<
	false,
	size_type>,
	false,
	size_type>
      type;
    };

    struct def_update_policy
    {
      typedef pb_assoc::move_to_front_lu_policy<> type;
    };

#ifdef __GNUC__
#undef PB_ASSOC_HASH_NAMESPACE
#endif // #ifdef __GNUC__

    template<class Comb_Probe_Fn>
    struct def_probe_fn
    {
    private:
      typedef typename Comb_Probe_Fn::size_type size_type;

    public:
      typedef
      typename pb_assoc::detail::cond_type<
	pb_assoc::detail::is_same_type<
	pb_assoc::direct_mask_range_hashing<size_t>,
	Comb_Probe_Fn>::value,
	pb_assoc::linear_probe_fn<
	size_type>,
	pb_assoc::quadratic_probe_fn<
	size_type> >::type
      type;
    };

    typedef pb_assoc::null_node_updator def_node_updator;

  } // namespace detail

} // namespace pb_assoc

#endif // #ifndef STANDARD_POLICIES_HPP

