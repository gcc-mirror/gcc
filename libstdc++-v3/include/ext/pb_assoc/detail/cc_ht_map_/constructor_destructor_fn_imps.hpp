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
 * @file constructor_destructor_fn_imps.hpp
 * Contains implementations of cc_ht_map_'s constructors, destructor,
 *	and related functions.
 */

PB_ASSOC_CLASS_T_DEC
typename PB_ASSOC_CLASS_C_DEC::entry_allocator
PB_ASSOC_CLASS_C_DEC::s_entry_allocator;

PB_ASSOC_CLASS_T_DEC
typename PB_ASSOC_CLASS_C_DEC::entry_pointer_allocator
PB_ASSOC_CLASS_C_DEC::s_entry_pointer_allocator;

PB_ASSOC_CLASS_T_DEC
template<class It>
void
PB_ASSOC_CLASS_C_DEC::
copy_from_range(It first_it, It last_it)
{
  while (first_it != last_it)
    insert(*(first_it++));
}

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
PB_ASSOC_CLASS_NAME() :
  my_ranged_hash_fn_base(Resize_Policy::get_init_size()),
  m_a_p_entries(s_entry_pointer_allocator.allocate(
						   Resize_Policy::get_init_size())),
  m_num_e_p(Resize_Policy::get_init_size()),
  m_num_used_e(0)
{
  std::fill(m_a_p_entries, m_a_p_entries + m_num_e_p, (entry_pointer)NULL);

  PB_ASSOC_DBG_ONLY(PB_ASSOC_CLASS_C_DEC::assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
PB_ASSOC_CLASS_NAME(const Hash_Fn& r_hash_fn) :
  my_ranged_hash_fn_base(Resize_Policy::get_init_size(),
			 r_hash_fn),
  m_a_p_entries(s_entry_pointer_allocator.allocate(
						   Resize_Policy::get_init_size())),
  m_num_e_p(Resize_Policy::get_init_size()),
  m_num_used_e(0)
{
  std::fill(m_a_p_entries, m_a_p_entries + m_num_e_p, (entry_pointer)NULL);

  PB_ASSOC_DBG_ONLY(PB_ASSOC_CLASS_C_DEC::assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
PB_ASSOC_CLASS_NAME(const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn) :
  PB_ASSOC_HASH_EQ_FN_C_DEC(r_eq_fn),
  my_ranged_hash_fn_base(Resize_Policy::get_init_size(),
			 r_hash_fn),
  m_a_p_entries(s_entry_pointer_allocator.allocate(
						   Resize_Policy::get_init_size())),
  m_num_e_p(Resize_Policy::get_init_size()),
  m_num_used_e(0)
{
  std::fill(m_a_p_entries, m_a_p_entries + m_num_e_p, (entry_pointer)NULL);

  PB_ASSOC_DBG_ONLY(PB_ASSOC_CLASS_C_DEC::assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
PB_ASSOC_CLASS_NAME(const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, const Comb_Hash_Fn& r_comb_hash_fn) :
  PB_ASSOC_HASH_EQ_FN_C_DEC(r_eq_fn),
  my_ranged_hash_fn_base(Resize_Policy::get_init_size(),
			 r_hash_fn, r_comb_hash_fn),
  m_a_p_entries(s_entry_pointer_allocator.allocate(
						   Resize_Policy::get_init_size())),
  m_num_e_p(Resize_Policy::get_init_size()),
  m_num_used_e(0)
{
  std::fill(m_a_p_entries, m_a_p_entries + m_num_e_p, (entry_pointer)NULL);

  PB_ASSOC_DBG_ONLY(PB_ASSOC_CLASS_C_DEC::assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
PB_ASSOC_CLASS_NAME(const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, const Comb_Hash_Fn& r_comb_hash_fn, const Resize_Policy& r_resize_policy) :
  PB_ASSOC_HASH_EQ_FN_C_DEC(r_eq_fn),
  Resize_Policy(r_resize_policy),
  my_ranged_hash_fn_base(Resize_Policy::get_init_size(),
			 r_hash_fn, r_comb_hash_fn),
  m_a_p_entries(s_entry_pointer_allocator.allocate(
						   Resize_Policy::get_init_size())),
  m_num_e_p(Resize_Policy::get_init_size()),
  m_num_used_e(0)
{
  std::fill(m_a_p_entries, m_a_p_entries + m_num_e_p, (entry_pointer)NULL);

  PB_ASSOC_DBG_ONLY(PB_ASSOC_CLASS_C_DEC::assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
PB_ASSOC_CLASS_NAME(const PB_ASSOC_CLASS_C_DEC& r_other) :
  PB_ASSOC_HASH_EQ_FN_C_DEC(r_other),
  my_resize_base(r_other),
  my_ranged_hash_fn_base(r_other),
  m_a_p_entries(s_entry_pointer_allocator.allocate(
						   r_other.m_num_e_p)),
  m_num_e_p(r_other.m_num_e_p),
  m_num_used_e(r_other.m_num_used_e)
{
  PB_ASSOC_DBG_ONLY(r_other.assert_valid();)

    std::fill(m_a_p_entries, m_a_p_entries + m_num_e_p, (entry_pointer)NULL);

  try
    {
      for (size_type i  = 0; i < m_num_e_p; ++i)
	{
	  entry_pointer p_e = r_other.m_a_p_entries[i];

	  while (p_e != NULL)
	    {
	      constructor_insert_new_imp(
					 const_mapped_reference(p_e->m_value), i,
					 my_hash_traits_base::s_store_hash_indicator);

	      p_e = p_e->m_p_next;
	    }
	}
    }
  catch(...)
    {
      deallocate_all();

      throw;
    }

  PB_ASSOC_DBG_ONLY(PB_ASSOC_CLASS_C_DEC::assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
~PB_ASSOC_CLASS_NAME()
{
  deallocate_all();
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
swap(PB_ASSOC_CLASS_C_DEC& r_other)
{
  PB_ASSOC_DBG_ONLY(assert_valid());
  PB_ASSOC_DBG_ONLY(r_other.assert_valid());

  std::swap(m_a_p_entries, r_other.m_a_p_entries);

  std::swap(m_num_e_p, r_other.m_num_e_p);

  std::swap(m_num_used_e, r_other.m_num_used_e);

  my_ranged_hash_fn_base::swap(r_other);

  my_hash_eq_fn_base::swap(r_other);

  my_resize_base::swap(r_other);

  PB_ASSOC_DBG_ONLY(my_map_debug_base::swap(r_other));

  PB_ASSOC_DBG_ONLY(assert_valid());
  PB_ASSOC_DBG_ONLY(r_other.assert_valid());
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
deallocate_all()
{
  clear();

  s_entry_pointer_allocator.deallocate(m_a_p_entries, m_num_e_p);
}

#include <ext/pb_assoc/detail/cc_ht_map_/constructor_destructor_store_hash_fn_imps.hpp>
#include <ext/pb_assoc/detail/cc_ht_map_/constructor_destructor_no_store_hash_fn_imps.hpp>
