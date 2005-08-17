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
 * @file cmp_fn_imps.hpp
 * Contains implementations of cc_ht_map_'s entire container comparison related
 *	functions.
 */

PB_ASSOC_CLASS_T_DEC
template<class Other_HT_Map_Type>
bool
PB_ASSOC_CLASS_C_DEC::
operator==(const Other_HT_Map_Type& r_other) const
{
  return (cmp_with_other(r_other));
}

PB_ASSOC_CLASS_T_DEC
template<class Other_Map_Type>
bool
PB_ASSOC_CLASS_C_DEC::
cmp_with_other(const Other_Map_Type& r_other) const
{
  if (size() != r_other.size())
    return (false);

  for (typename Other_Map_Type::const_iterator it = r_other.begin();
       it != r_other.end(); ++it)
    {
      const_key_reference r_key =(const_key_reference)PB_ASSOC_V2F(*it);

      const_mapped_pointer p_mapped_value =
	const_cast<PB_ASSOC_CLASS_C_DEC& >(*this).
	find_key_pointer(r_key, my_traits_base::m_store_hash_indicator);

      if (p_mapped_value == NULL)
	return (false);

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
      if (p_mapped_value->second != it->second)
	return (false);
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR
    }

  return (true);
}

PB_ASSOC_CLASS_T_DEC
template<class Other_HT_Map_Type>
bool
PB_ASSOC_CLASS_C_DEC::
operator!=(const Other_HT_Map_Type& r_other) const
{
  return (!operator==(r_other));
}
