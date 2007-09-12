// -*- C++ -*-

// Copyright (C) 2005, 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice
// and this permission notice appear in supporting documentation. None
// of the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied
// warranty.

/**
 * @file split_join_fn_imps.hpp
 * Containsert a random regression test for a specific container type.
 */

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
split_join()
{
  enum
    {
      split_join = container_traits::order_preserving
    };

  return (split_join_imp(__gnu_pbds::detail::integral_constant<int,split_join>()));
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
split_join_imp(__gnu_pbds::detail::false_type)
{
  return (true);
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
split_join_imp(__gnu_pbds::detail::true_type)
{
  PB_DS_TRACE("split_join");

  bool done = true;

  PB_DS_SET_DESTRUCT_PRINT

    try
      {
        m_alloc.set_throw_prob(0);

        Cntnr lhs(*m_p_c);

        Cntnr rhs;

        native_type native_lhs(m_native_c);

        native_type native_rhs;

        const key_type k =
	  test_traits::generate_key(m_g, m_m);

        m_alloc.set_throw_prob(m_tp);

        lhs.split(k, rhs);

        typename native_type::const_iterator it =
	  native_lhs.upper_bound(test_traits::native_key(k));

        while (!native_lhs.empty()&&  it != native_lhs.end())
	  {
            native_rhs.insert(*it);

            typename native_type::const_iterator next_it = it;
            ++next_it;

            native_lhs.erase(test_traits::extract_native_key(*it));

            it = next_it;
	  }

        PB_DS_COND_COMPARE(lhs, native_lhs);
        PB_DS_COND_COMPARE(rhs, native_rhs);

        m_alloc.set_throw_prob(m_tp);

        if (m_g.get_prob() < 0.5)
	  lhs.swap(rhs);

        lhs.join(rhs);

        PB_DS_THROW_IF_FAILED(
			      rhs.size() == 0,
			      static_cast<unsigned long>(rhs.size()),
			      m_p_c,
			      & m_native_c);

        PB_DS_THROW_IF_FAILED(
			      rhs.empty(),
			      static_cast<unsigned long>(rhs.size()),
			      m_p_c,
			      & m_native_c);

        m_p_c->swap(lhs);
      }
    catch(__gnu_cxx::forced_exception_error& )
      {
        done = false;

        PB_DS_THROW_IF_FAILED(            container_traits::split_join_can_throw, container_traits::split_join_can_throw, m_p_c, & m_native_c);
      }

  PB_DS_COND_COMPARE(*m_p_c, m_native_c);

  PB_DS_CANCEL_DESTRUCT_PRINT

    return (done);
}
