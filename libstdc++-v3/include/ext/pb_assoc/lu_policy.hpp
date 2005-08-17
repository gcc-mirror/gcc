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
 * @file lu_policy.hpp
 * Contains policies for list update containers.
 */

#ifndef LU_POLICY_HPP
#define LU_POLICY_HPP

namespace pb_assoc
{
  struct move_to_front_lu_metadata
  { };

#define PB_ASSOC_CLASS_T_DEC \
	template<typename Metadata_Reference>

#define PB_ASSOC_CLASS_C_DEC \
	move_to_front_lu_policy<Metadata_Reference>

  template<typename Metadata_Reference = 
           std::allocator<move_to_front_lu_metadata>::reference>
    class move_to_front_lu_policy
    {
    public:
      void
      swap(PB_ASSOC_CLASS_C_DEC& r_other);
      
    protected:
      typedef move_to_front_lu_metadata metadata_type;
      
      typedef Metadata_Reference metadata_reference;
      
      metadata_type
      operator()() const;
      
      inline bool
      operator()(metadata_reference r_data) const;
    };

#include <ext/pb_assoc/detail/lu_policy/mtf_lu_policy_imp.hpp>

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

  template<typename Size_Type>
    class counter_lu_policy_base;

  template<typename Size_Type =	size_t>
    class counter_lu_metadata
    {
    public:
      typedef Size_Type size_type;

  private:
      counter_lu_metadata(size_type init_count) : m_count(init_count)
      { }
      
      mutable size_type m_count;
      
      friend class counter_lu_policy_base<Size_Type>;
    };

  template<typename Size_Type>
    class counter_lu_policy_base;

#define PB_ASSOC_CLASS_T_DEC \
	template<typename Size_Type>

#define PB_ASSOC_CLASS_C_DEC \
	counter_lu_policy_base<Size_Type>

  template<typename Size_Type>
    class counter_lu_policy_base
    {
    protected:
      typedef Size_Type size_type;
      
    counter_lu_metadata<Size_Type>
    operator()(size_type max_size) const;

    template<typename Metadata_Reference>
      bool
      operator()(Metadata_Reference r_data, size_type m_max_count) const;
    };

#include <ext/pb_assoc/detail/lu_policy/counter_lu_metadata_imp.hpp>

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template<typename Size_Type, typename Metadata_Reference>

#define PB_ASSOC_CLASS_C_DEC \
	counter_lu_policy<Size_Type, Metadata_Reference>

  template<typename Size_Type = size_t,
	   typename Metadata_Reference =
	   typename std::allocator<counter_lu_metadata<Size_Type> >::reference>
    class counter_lu_policy : private counter_lu_policy_base<Size_Type>
    {
    public:
      typedef Size_Type size_type;
      
      counter_lu_policy(size_type max_count = 5);
      
      void
      swap(PB_ASSOC_CLASS_C_DEC& r_other);
      
      inline
      size_type
      get_max_count() const;
      
    protected:
      
      typedef counter_lu_metadata< Size_Type> metadata_type;
      
      typedef Metadata_Reference metadata_reference;
      
      metadata_type
      operator()() const;
      
      bool
      operator()(metadata_reference r_data) const;
      
    private:
      typedef counter_lu_policy_base< Size_Type> my_base;
      
      size_type m_max_count;
    };

#include <ext/pb_assoc/detail/lu_policy/counter_lu_policy_imp.hpp>

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

} // namespace pb_assoc

#endif // #ifndef LU_POLICY_HPP
