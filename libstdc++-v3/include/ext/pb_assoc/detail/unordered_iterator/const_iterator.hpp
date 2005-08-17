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
 * @file const_iterator.hpp
 * Contains an iterator class used for const ranging over the elements of the
 *   table.
 */

class const_iterator_ : 
  public const_find_iterator_

{

public:

  typedef std::forward_iterator_tag iterator_category;

  typedef typename Allocator::difference_type difference_type;

  typedef mapped_value_type value_type;

  typedef mapped_pointer pointer;

  typedef const_mapped_pointer const_pointer;

  typedef mapped_reference reference;

  typedef const_mapped_reference const_reference;

public:

  inline
  const_iterator_()

    : m_p_tbl(NULL)
  { }

  inline const_iterator_& 
  operator++()
  {
    m_p_tbl->inc_it_state(my_base::m_p_value, m_pos);

    return (*this);
  }

  inline const_iterator_
  operator++(int)
  {
    const_iterator_ ret =* this;

    m_p_tbl->inc_it_state(my_base::m_p_value, m_pos);

    return (ret);
  }

protected:

  typedef const_find_iterator_ my_base;

protected:

  /**
   * Constructor used by the table to initiate the generalized
   *   pointer and position (e.g., this is called from within a find()
   *   of a table.
   **/
  inline
  const_iterator_(const_mapped_pointer p_value, PB_ASSOC_GEN_POS pos, const PB_ASSOC_CLASS_C_DEC* p_tbl) : const_find_iterator_(p_value),
													   m_p_tbl(p_tbl),
													   m_pos(pos)
  { }

protected:

  /**
   * Pointer to the table object which created the iterator (used for
   *   incrementing its position.
   **/
  const PB_ASSOC_CLASS_C_DEC* m_p_tbl;

  PB_ASSOC_GEN_POS m_pos;

  friend class PB_ASSOC_CLASS_C_DEC;
};

