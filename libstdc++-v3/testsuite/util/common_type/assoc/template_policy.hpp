// -*- C++ -*-

// Copyright (C) 2005-2014 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.


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
 * @file template_policy.hpp
 * Contains template versions of policies.
 */

#ifndef PB_DS_TEMPLATE_POLICY_HPP
#define PB_DS_TEMPLATE_POLICY_HPP

#include <ext/typelist.h>
#include <ext/pb_ds/hash_policy.hpp>
#include <ext/pb_ds/tree_policy.hpp>
#include <ext/pb_ds/list_update_policy.hpp>

namespace __gnu_pbds
{
  namespace test
  {
    template<typename _Alloc>
    struct direct_mask_range_hashing_t_ 
    : public __gnu_pbds::direct_mask_range_hashing<typename _Alloc::size_type>
    {
      typedef typename _Alloc::size_type size_type;
      typedef __gnu_pbds::direct_mask_range_hashing<size_type> base_type;
    };

    template<typename _Alloc>
    struct direct_mod_range_hashing_t_ 
    : public __gnu_pbds::direct_mod_range_hashing<typename _Alloc::size_type>
    {
      typedef typename _Alloc::size_type size_type;
      typedef __gnu_pbds::direct_mod_range_hashing<size_type> base_type;
    };

    template<typename _Alloc,
	     typename _Alloc::size_type Min_Load_Nom,
	     typename _Alloc::size_type Min_Load_Denom,
	     typename _Alloc::size_type Max_Load_Nom,
	     typename _Alloc::size_type Max_Load_Denom,
	     bool External_Access>
    struct hash_load_check_resize_trigger_t_ 
    : public __gnu_pbds::hash_load_check_resize_trigger<External_Access,
						   typename _Alloc::size_type>
    {
      typedef typename _Alloc::size_type size_type;
      typedef __gnu_pbds::hash_load_check_resize_trigger<External_Access, size_type>  base_type;

      inline
      hash_load_check_resize_trigger_t_() 
      : base_type(static_cast<float>(Min_Load_Nom) / static_cast<float>(Min_Load_Denom), static_cast<float>(Max_Load_Nom) / static_cast<float>(Max_Load_Denom))
      { }

      enum
	{
	  get_set_loads = External_Access,
	  get_set_load = false
	};
    };

    template<typename _Alloc,
	     typename _Alloc::size_type Load_Nom,
	     typename _Alloc::size_type Load_Denom,
	     bool External_Access>
    struct cc_hash_max_collision_check_resize_trigger_t_ 
    : public __gnu_pbds::cc_hash_max_collision_check_resize_trigger<External_Access,
      typename _Alloc::size_type>
    {
      typedef typename _Alloc::size_type size_type;
      typedef __gnu_pbds::cc_hash_max_collision_check_resize_trigger<External_Access, size_type> base_type;

      inline
      cc_hash_max_collision_check_resize_trigger_t_() 
      : base_type(static_cast<float>(Load_Nom) / static_cast<float>(Load_Denom))
      { }

      enum
	{
	  get_set_load = External_Access,
	  get_set_loads = false
	};
    };

    struct hash_prime_size_policy_t_ : public __gnu_pbds::hash_prime_size_policy
    { };

    template<typename _Alloc>
    struct hash_exponential_size_policy_t_ 
    : public __gnu_pbds::hash_exponential_size_policy<typename _Alloc::size_type>
    { };

    template<typename Key, typename _Alloc>
    struct linear_probe_fn_t_ 
    : public __gnu_pbds::linear_probe_fn<typename _Alloc::size_type>
    { };

    template<typename Key, typename _Alloc>
    struct quadratic_probe_fn_t_ 
    : public __gnu_pbds::quadratic_probe_fn<typename _Alloc::size_type>
    { };

    template<typename _Alloc, typename _Alloc::size_type Max_Count>
    struct lu_counter_policy_t_ 
    : public __gnu_pbds::lu_counter_policy<Max_Count, _Alloc>
    {
      typedef __gnu_pbds::lu_counter_policy<Max_Count, _Alloc> base_type;
    };

    struct lu_move_to_front_policy_t_ 
    : public __gnu_pbds::lu_move_to_front_policy<>
    { };
  } // namespace test
} // namespace __gnu_pbds

#endif 

