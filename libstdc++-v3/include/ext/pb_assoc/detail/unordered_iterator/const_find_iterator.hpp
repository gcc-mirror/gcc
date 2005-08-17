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
 * @file const_find_iterator.hpp
 * Contains an iterator class returned by the tables' const find and insert
 *   methods.
 */

class find_iterator_;

class const_find_iterator_
{

public:

  typedef trivial_iterator_tag iterator_category;

  typedef trivial_iterator_difference_type difference_type;

  typedef mapped_value_type value_type;

  typedef mapped_pointer pointer;

  typedef const_mapped_pointer const_pointer;

  typedef mapped_reference reference;

  typedef const_mapped_reference const_reference;

public:

  inline
  const_find_iterator_(const_pointer p_value) : m_p_value(p_value)
  { }

  inline
  const_find_iterator_()

    : m_p_value(NULL)
  { }

  inline
  const_find_iterator_(const const_find_iterator_& r_other)

    : m_p_value(r_other.m_p_value)
  { }

  inline
  const_find_iterator_(const find_iterator_& r_other)

    : m_p_value(r_other.m_p_value)
  { }

  inline const_pointer
  operator->() const
  {
    PB_ASSOC_DBG_ASSERT(m_p_value != NULL);

    return (m_p_value);
  }

  inline const_reference
  operator*() const
  {
    PB_ASSOC_DBG_ASSERT(m_p_value != NULL);

    return (*m_p_value);
  }

  inline bool
  operator==(const find_iterator_& r_other) const
  {
    return (m_p_value == r_other.m_p_value);
  }

  inline bool
  operator==(const const_find_iterator_& r_other) const
  {
    return (m_p_value == r_other.m_p_value);
  }

  inline bool
  operator!=(const find_iterator_& r_other) const
  {
    return (m_p_value != r_other.m_p_value);
  }

  inline bool
  operator!=(const const_find_iterator_& r_other) const
  {
    return (m_p_value != r_other.m_p_value);
  }

protected:
  const_pointer m_p_value;

  friend class find_iterator_;

  friend class PB_ASSOC_CLASS_C_DEC;
};

