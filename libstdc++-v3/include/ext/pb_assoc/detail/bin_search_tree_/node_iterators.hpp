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
 * @file node_iterators.hpp
 * Contains an implementation class for bin_search_tree_.
 */

class const_node_it_
{

public:

  typedef trivial_iterator_tag iterator_category;

  typedef trivial_iterator_difference_type difference_type;

  typedef const_iterator value_type;

  typedef const_iterator*  pointer;

  typedef const_iterator*  const_pointer;

  typedef const_iterator&  reference;

  typedef const iterator&  const_reference;

public:

  inline
  const_node_it_(const node_pointer p_nd = NULL)

    : m_p_nd(const_cast<node_pointer>(p_nd))
  { }

  inline const_iterator
  operator*() const
  {
    return (const_iterator(m_p_nd));
  }

  inline const_node_it_
  l_child() const
  {
    return (const_node_it_(m_p_nd->m_p_left));
  }

  inline const_node_it_
  r_child() const
  {
    return (const_node_it_(m_p_nd->m_p_right));
  }

  inline bool
  operator==(const const_node_it_& r_other) const
  {
    return (m_p_nd == r_other.m_p_nd);
  }

  inline bool
  operator!=(const const_node_it_& r_other) const
  {
    return (m_p_nd != r_other.m_p_nd);
  }

private:

  friend class PB_ASSOC_CLASS_C_DEC;

public:
  node_pointer m_p_nd;
};

class node_it_ : 
  public const_node_it_

{

public:

  inline
  node_it_(const node_pointer p_nd = NULL)

    : const_node_it_(const_cast<node_pointer>(p_nd))
  { }

  inline iterator
  operator*() const
  {
    return (iterator(const_node_it_::m_p_nd));
  }

  inline node_it_
  l_child()
  {
    return (node_it_(const_node_it_::m_p_nd->m_p_left));
  }

  inline node_it_
  r_child()
  {
    return (node_it_(const_node_it_::m_p_nd->m_p_right));
  }

private:
  friend class PB_ASSOC_CLASS_C_DEC;
};

