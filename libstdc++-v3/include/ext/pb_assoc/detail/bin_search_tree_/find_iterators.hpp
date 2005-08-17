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
 * @file find_iterators.hpp
 * Contains an implementation class for bin_search_tree_.
 */

#define PB_ASSOC_CONST_IT_C_DEC \
	const_it_< \
		Is_Forward_Iterator>

#define PB_ASSOC_CONST_ODIR_IT_C_DEC \
	const_it_< \
		!Is_Forward_Iterator>

#define PB_ASSOC_IT_C_DEC \
	it_< \
		Is_Forward_Iterator>

#define PB_ASSOC_ODIR_IT_C_DEC \
	it_< \
		!Is_Forward_Iterator>

template<bool Is_Forward_Iterator>
class const_it_
{

public:

  typedef std::bidirectional_iterator_tag iterator_category;

  typedef typename Allocator::difference_type difference_type;

  typedef mapped_value_type value_type;

  typedef mapped_pointer pointer;

  typedef const_mapped_pointer const_pointer;

  typedef mapped_reference reference;

  typedef const_mapped_reference const_reference;

public:

  inline
  const_it_(const node_pointer p_nd = NULL) : m_p_nd(const_cast<node_pointer>(p_nd))
  { }

  inline
  const_it_(const PB_ASSOC_CONST_ODIR_IT_C_DEC& 
	    r_other)

    : m_p_nd(r_other.m_p_nd)
  { }

  inline
  PB_ASSOC_CONST_IT_C_DEC& 
  operator=(const PB_ASSOC_CONST_IT_C_DEC& 
	    r_other)
  {
    m_p_nd = r_other.m_p_nd;

    return (*this);
  }

  inline
  PB_ASSOC_CONST_IT_C_DEC& 
  operator=(const PB_ASSOC_CONST_ODIR_IT_C_DEC& 
	    r_other)
  {
    m_p_nd = r_other.m_p_nd;

    return (*this);
  }

  inline const_pointer
  operator->() const
  {
    PB_ASSOC_DBG_ASSERT(m_p_nd != NULL);

    return (&m_p_nd->m_value);
  }

  inline const_reference
  operator*() const
  {
    PB_ASSOC_DBG_ASSERT(m_p_nd != NULL);

    return (m_p_nd->m_value);
  }

  inline bool
  operator==(const PB_ASSOC_CONST_IT_C_DEC
	     &r_other) const
  {
    return (m_p_nd == r_other.m_p_nd);
  }

  inline bool
  operator==(const PB_ASSOC_CONST_ODIR_IT_C_DEC
	     &r_other) const
  {
    return (m_p_nd == r_other.m_p_nd);
  }

  inline bool
  operator!=(const PB_ASSOC_CONST_IT_C_DEC& 
	     r_other) const
  {
    return (m_p_nd != r_other.m_p_nd);
  }

  inline bool
  operator!=(const PB_ASSOC_CONST_ODIR_IT_C_DEC& 
	     r_other) const
  {
    return (m_p_nd != r_other.m_p_nd);
  }

  inline PB_ASSOC_CONST_IT_C_DEC& 
  operator++()
  {
    PB_ASSOC_DBG_ASSERT(m_p_nd != NULL);

    inc(int_to_type<Is_Forward_Iterator>());

    return (*this);
  }

  inline PB_ASSOC_CONST_IT_C_DEC
  operator++(int)
  {
    PB_ASSOC_CONST_IT_C_DEC
      ret_it(m_p_nd);

    operator++();

    return (ret_it);
  }

  inline PB_ASSOC_CONST_IT_C_DEC& 
  operator--()
  {
    dec(int_to_type<Is_Forward_Iterator>());

    return (*this);
  }

  inline PB_ASSOC_CONST_IT_C_DEC
  operator--(int)
  {
    PB_ASSOC_CONST_IT_C_DEC
      ret_it(m_p_nd);

    operator--();

    return (ret_it);
  }

protected:
  inline void
  inc(int_to_type<false>)
  {
    dec(int_to_type<true>());
  }

  void
  inc(int_to_type<true>)
  {
    if (m_p_nd->m_p_right != NULL)
      {
	m_p_nd = m_p_nd->m_p_right;

	while (m_p_nd->m_p_left != NULL)
	  m_p_nd = m_p_nd->m_p_left;

	return;
      }

    node_pointer p_y = m_p_nd->m_p_parent;

    while (m_p_nd == p_y->m_p_right)
      {
	m_p_nd = p_y;

	p_y = p_y->m_p_parent;
      }

    if (m_p_nd->m_p_right != p_y)
      m_p_nd = p_y;
  }

  inline void
  dec(int_to_type<false>)
  {
    inc(int_to_type<true>());
  }

  void
  dec(int_to_type<true>)
  {
    if (m_p_nd->special_dec_check()&& 
	m_p_nd->m_p_parent->m_p_parent == m_p_nd)
      {
	m_p_nd = m_p_nd->m_p_right;

	return;
      }

    if (m_p_nd->m_p_left != NULL)
      {
	node_pointer p_y = m_p_nd->m_p_left;

	while (p_y->m_p_right != NULL)
	  p_y = p_y->m_p_right;

	m_p_nd = p_y;

	return;
      }

    node_pointer p_y = m_p_nd->m_p_parent;

    while (m_p_nd == p_y->m_p_left)
      {
	m_p_nd = p_y;

	p_y = p_y->m_p_parent;
      }

    /*
     * This seems to correct an apparent bug in the SGI STL
     * implementation. */
    if (m_p_nd->m_p_left != p_y)
      m_p_nd = p_y;
  }

  friend class PB_ASSOC_CLASS_C_DEC;

public:
  node_pointer m_p_nd;
};

template<bool Is_Forward_Iterator>
class it_ : 
  public PB_ASSOC_CONST_IT_C_DEC

{

public:

  inline
  it_(const node_pointer p_nd = NULL) : PB_ASSOC_CONST_IT_C_DEC((node_pointer)p_nd)
  { }

  inline
  it_(const PB_ASSOC_ODIR_IT_C_DEC& 
      r_other)

    : PB_ASSOC_CONST_IT_C_DEC(
			      r_other.m_p_nd)
  { }

  inline
  PB_ASSOC_IT_C_DEC& 
  operator=(const PB_ASSOC_IT_C_DEC& 
	    r_other)
  {
    my_base_it::m_p_nd = r_other.m_p_nd;

    return (*this);
  }

  inline
  PB_ASSOC_IT_C_DEC& 
  operator=(const PB_ASSOC_ODIR_IT_C_DEC& 
	    r_other)
  {
    my_base_it::m_p_nd = r_other.m_p_nd;

    return (*this);
  }

  inline pointer
  operator->()
  {
    PB_ASSOC_DBG_ASSERT(my_base_it::m_p_nd != NULL);

    return (&my_base_it::m_p_nd->m_value);
  }

  inline reference
  operator*()
  {
    PB_ASSOC_DBG_ASSERT(my_base_it::m_p_nd != NULL);

    return (my_base_it::m_p_nd->m_value);
  }

  inline PB_ASSOC_IT_C_DEC& 
  operator++()
  {
    PB_ASSOC_CONST_IT_C_DEC::
      operator++();

    return (*this);
  }

  inline PB_ASSOC_IT_C_DEC
  operator++(int)
  {
    PB_ASSOC_IT_C_DEC
      ret_it(my_base_it::m_p_nd);

    operator++();

    return (ret_it);
  }

  inline PB_ASSOC_IT_C_DEC& 
  operator--()
  {
    PB_ASSOC_CONST_IT_C_DEC::
      operator--();

    return (*this);
  }

  inline PB_ASSOC_IT_C_DEC
  operator--(int)
  {
    PB_ASSOC_IT_C_DEC
      ret_it(my_base_it::m_p_nd);

    operator--();

    return (ret_it);
  }

protected:
  typedef PB_ASSOC_CONST_IT_C_DEC my_base_it;

  friend class PB_ASSOC_CLASS_C_DEC;
};

#undef PB_ASSOC_CONST_IT_C_DEC

#undef PB_ASSOC_CONST_ODIR_IT_C_DEC

#undef PB_ASSOC_IT_C_DEC

#undef PB_ASSOC_ODIR_IT_C_DEC

