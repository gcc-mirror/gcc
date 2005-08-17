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
 * @file it_.hpp
 * Contains an it_ for an adapter of mapping levels.
 */

#define PB_ASSOC_IT_C_DEC \
	it_< \
		It0, \
		It1, \
		Has_Data, \
		Const>

#define PB_ASSOC_OIT_T_DEC \
	template<class OIt0, class OIt1, bool OHas_Data, bool OConst>

#define PB_ASSOC_OIT_C_DEC \
	it_< \
		OIt0, \
		OIt1, \
		OHas_Data, \
		OConst>

template<class It0, class It1, bool Has_Data, bool Const>
class it_
{
public:

  typedef typename it_value_type_traits_t::value_type value_type;

  typedef typename it_value_type_traits_t::reference reference;

  typedef
  typename it_value_type_traits_t::const_reference
  const_reference;

  typedef typename it_value_type_traits_t::pointer pointer;

  typedef typename it_value_type_traits_t::const_pointer const_pointer;

public:
  inline
  it_(It0 it0 = It0(),
      It0 end_it0 = It0(),
      It1 it1 = It1()) : m_it0(it0),
			 m_end_it0(end_it0),
			 m_it1(it1)
  { }

  inline
  it_(const PB_ASSOC_IT_C_DEC& r_other) : m_it0(r_other.m_it0),
					  m_end_it0(r_other.m_end_it0),
					  m_it1(r_other.m_it1)
  { }

  PB_ASSOC_OIT_T_DEC
  inline
  it_(const PB_ASSOC_OIT_C_DEC& r_other) : m_it0(r_other.m_it0),
					   m_end_it0(r_other.m_end_it0),
					   m_it1(r_other.m_it1)
  { }

  inline bool
  operator==(const PB_ASSOC_IT_C_DEC& r_other) const
  {
    if (m_it0 != r_other.m_it0)
      return (false);

    if (m_it0 == m_end_it0)
      return (true);

    return (m_it1 == r_other.m_it1);
  }

  inline bool
  operator!=(const PB_ASSOC_IT_C_DEC& r_other) const
  {
    return (!operator==(r_other));
  }

  inline PB_ASSOC_IT_C_DEC& 
  operator++()
  {
    ++m_it1;

    if (m_it1 == m_it0->second.end())
      do
	{
	  ++m_it0;
	}
      while (m_it0 != m_end_it0&&  m_it0->second.empty());

    if (m_it0 != m_end_it0&&  !m_it0->second.empty())
      m_it1 = m_it0->second.begin();

    return (*this);
  }

  inline PB_ASSOC_IT_C_DEC
  operator++(int)
  {
    PB_ASSOC_IT_C_DEC ret =* this;

    operator++();

    return (ret);
  }

  inline const_pointer
  operator->() const
  {
    it_value_type_traits_t::make_valid(m_value_type_holder, m_it0->first, * m_it1);

    return (it_value_type_traits_t::recast(m_value_type_holder));
  }

  inline pointer
  operator->()
  {
    // Tmp Ami PB_ASSOC_STATIC_ASSERT(non_const, !Const);

    it_value_type_traits_t::make_valid(m_value_type_holder, m_it0->first, * m_it1);

    return (it_value_type_traits_t::recast(m_value_type_holder));
  }

  inline const_reference
  operator*() const
  {
    return (*operator->());
  }

  inline reference
  operator*()
  {
    PB_ASSOC_STATIC_ASSERT(non_const, !Const);

    return (*operator->());
  }

public:
  mutable It0 m_it0;
  It0 m_end_it0;

  mutable It1 m_it1;

  int_to_type<Has_Data> m_has_data;

private:
  mutable typename it_value_type_traits_t::value_type_holder m_value_type_holder;
};

#undef PB_ASSOC_IT_C_DEC

#undef PB_ASSOC_OIT_T_DEC

#undef PB_ASSOC_OIT_C_DEC

