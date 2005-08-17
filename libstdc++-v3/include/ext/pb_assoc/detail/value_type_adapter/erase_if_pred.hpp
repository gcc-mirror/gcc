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
 * @file erase_if_pred.hpp
 * Contains a bridge erase_if predicate
 */

template<class Pred>
class erase_if_pred
{
public:
  // Tmp Ami all erase-ifs should take a reference to a predicate?
  erase_if_pred(Pred& r_pred, typename my_base::iterator base_it) : m_r_pred(r_pred),
								    m_base_it(base_it)
  { }

  inline bool
  operator()(typename my_base_data_type::iterator::reference r_val)
  {
    it_value_type_traits_t::make_valid(m_value_type_holder, m_base_it->first, r_val);

    typename it_value_type_traits_t::pointer p_exp_val =
      it_value_type_traits_t::recast(m_value_type_holder);

    return (m_r_pred(*p_exp_val));
  }

private:
  Pred& m_r_pred;

  typename my_base::iterator m_base_it;

  typename it_value_type_traits_t::value_type_holder m_value_type_holder;
};

