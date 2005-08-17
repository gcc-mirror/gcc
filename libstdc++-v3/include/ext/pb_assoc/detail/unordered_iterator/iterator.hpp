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
 * @file iterator.hpp
 * Contains an iterator_ class used for ranging over the elements of the
 *	table.
 */

class iterator_ : 
  public const_iterator_

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
  iterator_()

    : const_iterator_(NULL, PB_ASSOC_GEN_POS(), NULL)
  { }

  inline
  operator find_iterator_()
  {
    return (find_iterator_(
			   const_cast<pointer>(const_iterator_::m_p_value)));
  }

  inline
  operator const find_iterator_() const
  {
    return (find_iterator_(
			   const_cast<pointer>(const_iterator_::m_p_value)));
  }

  inline pointer
  operator->()
  {
    PB_ASSOC_DBG_ASSERT(my_base::m_p_value != NULL);

    return (const_cast<pointer>(my_base::m_p_value));
  }

  inline reference
  operator*()
  {
    PB_ASSOC_DBG_ASSERT(my_base::m_p_value != NULL);

    return (*(operator->()));
  }

  inline iterator_& 
  operator++()
  {
    my_base::m_p_tbl->inc_it_state(my_base::m_p_value, my_base::m_pos);

    return (*this);
  }

  inline iterator_
  operator++(int)
  {
    iterator_ ret =* this;

    my_base::m_p_tbl->inc_it_state(my_base::m_p_value, my_base::m_pos);

    return (ret);
  }

protected:
  typedef const_iterator_ my_base;

protected:

  /**
   * Constructor used by the table to initiate the generalized
   *   pointer and position (e.g., this is called from within a find()
   *   of a table.
   **/
  inline
  iterator_(pointer p_value, PB_ASSOC_GEN_POS pos, PB_ASSOC_CLASS_C_DEC* p_tbl) : const_iterator_(p_value, pos, p_tbl)
  { }

  friend class PB_ASSOC_CLASS_C_DEC;
};

