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
 * Contains implementations of cc_ht's constructors, destructor,
 *	and related functions.
 */

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
cc_hash_assoc_cntnr()
{ }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
cc_hash_assoc_cntnr(const Hash_Fn& r_hash_fn) :
  my_base(r_hash_fn)
{ }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
cc_hash_assoc_cntnr(const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn) :
  my_base(r_hash_fn, r_eq_fn)
{ }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
cc_hash_assoc_cntnr(const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, const Comb_Hash_Fn& r_comb_hash_fn) :
  my_base(r_hash_fn, r_eq_fn, r_comb_hash_fn)
{ }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
cc_hash_assoc_cntnr(const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, const Comb_Hash_Fn& r_comb_hash_fn, const Resize_Policy& r_resize_policy) :
  my_base(r_hash_fn, r_eq_fn, r_comb_hash_fn, r_resize_policy)
{ }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC& 
PB_ASSOC_CLASS_C_DEC::
operator=(const PB_ASSOC_CLASS_C_DEC& r_other)
{
  if (this !=& r_other)
    {
      PB_ASSOC_CLASS_C_DEC tmp(r_other);

      swap(tmp);
    }

  return (*this);
}

PB_ASSOC_CLASS_T_DEC
template<class It>
PB_ASSOC_CLASS_C_DEC::
cc_hash_assoc_cntnr(It first_it, It last_it)
{
  my_base::copy_from_range(first_it, last_it);
}

PB_ASSOC_CLASS_T_DEC
template<class It>
PB_ASSOC_CLASS_C_DEC::
cc_hash_assoc_cntnr(It first_it, It last_it, const Hash_Fn& r_hash_fn) :
  my_base(r_hash_fn)
{
  copy_from_range(first_it, last_it);
}

// Tmp Ami make all methods in assoc_cntnr.hpp inline

PB_ASSOC_CLASS_T_DEC
template<class It>
PB_ASSOC_CLASS_C_DEC::
cc_hash_assoc_cntnr(It first_it, It last_it, const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn) :
  my_base(r_hash_fn, r_eq_fn)
{
  copy_from_range(first_it, last_it);
}

PB_ASSOC_CLASS_T_DEC
template<class It>
PB_ASSOC_CLASS_C_DEC::
cc_hash_assoc_cntnr(It first_it, It last_it, const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, const Comb_Hash_Fn& r_comb_hash_fn) :
  my_base(r_hash_fn, r_eq_fn, r_comb_hash_fn)
{
  copy_from_range(first_it, last_it);
}

PB_ASSOC_CLASS_T_DEC
template<class It>
PB_ASSOC_CLASS_C_DEC::
cc_hash_assoc_cntnr(It first_it, It last_it, const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, const Comb_Hash_Fn& r_comb_hash_fn, const Resize_Policy& r_resize_policy) :
  my_base(r_hash_fn, r_eq_fn, r_comb_hash_fn, r_resize_policy)
{
  copy_from_range(first_it, last_it);
}

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
cc_hash_assoc_cntnr(const PB_ASSOC_CLASS_C_DEC& r_other) :
  PB_ASSOC_BASE_C_DEC((const PB_ASSOC_BASE_C_DEC& )r_other)
{ }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
~cc_hash_assoc_cntnr()
{ }

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
swap(PB_ASSOC_CLASS_C_DEC& r_other)
{
  my_base::swap(r_other);
}
