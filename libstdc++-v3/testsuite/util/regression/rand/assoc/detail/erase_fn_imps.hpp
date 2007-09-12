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
 * @file erase_fn_imps.hpp
 * Containsert a random regression test for a specific container type.
 */

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
erase()
{
  PB_DS_TRACE("erase");

  bool done = true;

  PB_DS_SET_DESTRUCT_PRINT

    m_alloc.set_throw_prob(0);

  const key_type k =
    test_traits::generate_key(m_g, m_m);

  m_alloc.set_throw_prob(m_tp);

  try
    {
      const bool cntnd = m_p_c->find(k) != m_p_c->end();

      PB_DS_THROW_IF_FAILED(
			    cntnd ==(m_native_c.find(test_traits::native_key(k)) != m_native_c.end()),
			    test_traits::key_to_string(k),
			    m_p_c,
			    & m_native_c);

      const bool ersd = m_p_c->erase(k);

      const bool native_ersd =
	m_native_c.erase(test_traits::native_key(k)) != 0;

      PB_DS_THROW_IF_FAILED(
			    ersd == native_ersd,
			    ersd << " " << native_ersd,
			    m_p_c,
			    & m_native_c);

      PB_DS_THROW_IF_FAILED(
			    m_p_c->find(k) == m_p_c->end(),
			    "",
			    m_p_c,
			    & m_native_c);
    }
  catch(__gnu_cxx::forced_exception_error& )
    {
      done = false;

      PB_DS_THROW_IF_FAILED(            container_traits::erase_can_throw, container_traits::erase_can_throw, m_p_c, & m_native_c);
    }

  PB_DS_COND_COMPARE(*m_p_c, m_native_c);

  PB_DS_CANCEL_DESTRUCT_PRINT

    return (done);
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
erase_if()
{
  PB_DS_TRACE("erase_if");

  bool done = true;

  PB_DS_SET_DESTRUCT_PRINT

    try
      {
        typedef
	  typename std::iterator_traits<
	  typename cntnr::iterator>::reference
	  it_const_reference;

        typedef
	  typename test_traits::template erase_if_fn<
	  value_type>
	  erase_if_fn_t;

        m_alloc.set_throw_prob(m_tp);

        const size_t ersd =
	  m_p_c->erase_if(erase_if_fn_t());

        const size_t native_ersd =
	  test_traits::erase_if(m_native_c);

        PB_DS_THROW_IF_FAILED(
			      ersd == native_ersd,
			      static_cast<unsigned long>(ersd) << " " <<
			      static_cast<unsigned long>(native_ersd),
			      m_p_c,
			      & m_native_c);
      }
    catch(__gnu_cxx::forced_exception_error& )
      {
        done = false;

        PB_DS_THROW_IF_FAILED(            container_traits::erase_can_throw, container_traits::erase_can_throw, m_p_c, & m_native_c);
      }

  PB_DS_COND_COMPARE(*m_p_c, m_native_c);

  PB_DS_CANCEL_DESTRUCT_PRINT

    return (done);
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
erase_it()
{
  enum
    {
      erase_iterators =
      container_traits::order_preserving
    };

  return (erase_it_imp(__gnu_pbds::detail::integral_constant<int,erase_iterators>()));
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
erase_it_imp(__gnu_pbds::detail::false_type)
{
  return (true);
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
erase_it_imp(__gnu_pbds::detail::true_type)
{
  PB_DS_TRACE("erase_it");

  bool done = true;

  PB_DS_SET_DESTRUCT_PRINT

    try
      {
        m_alloc.set_throw_prob(0);

        const key_type k =
	  test_traits::generate_key(m_g, m_m);

        m_alloc.set_throw_prob(m_tp);

        typename cntnr::iterator found_it = m_p_c->find(k);

        typename native_type::iterator native_it = m_native_c.find(
								   test_traits::native_key(k));

        const bool found = found_it != m_p_c->end();
        const bool native_found = native_it != m_native_c.end();

        PB_DS_THROW_IF_FAILED(
			      found == native_found,
			      found << " " <<    native_found,
			      m_p_c,
			      & m_native_c);

        typename cntnr::const_iterator next_it = found_it;
        if (next_it != m_p_c->end())
	  ++next_it;

        typename cntnr::iterator next_ers_it =
	  m_p_c->erase(found_it);

        if (native_it != m_native_c.end())
	  m_native_c.erase(native_it);

        bool range_guarantee =
	  __gnu_pbds::detail::is_same<
	  typename container_traits::invalidation_guarantee,
	  __gnu_pbds::range_invalidation_guarantee>::value ;

        if (range_guarantee)
	  PB_DS_THROW_IF_FAILED(                next_ers_it == next_it,  "",  m_p_c, & m_native_c);
      }
    catch(__gnu_cxx::forced_exception_error& )
      {
        done = false;

        PB_DS_THROW_IF_FAILED(            container_traits::erase_can_throw, container_traits::erase_can_throw, m_p_c, & m_native_c);
      }

  PB_DS_COND_COMPARE(*m_p_c, m_native_c);

  PB_DS_CANCEL_DESTRUCT_PRINT

    return (done);
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
erase_rev_it()
{
  enum
    {
      erase_iterators =
      container_traits::order_preserving&& 
      container_traits::reverse_iteration
    };

  return (erase_rev_it_imp(__gnu_pbds::detail::integral_constant<int,erase_iterators>()));
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
erase_rev_it_imp(__gnu_pbds::detail::false_type)
{
  return (true);
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
erase_rev_it_imp(__gnu_pbds::detail::true_type)
{
  PB_DS_TRACE("erase_rev_it");

  bool done = true;

  PB_DS_SET_DESTRUCT_PRINT

    try
      {
        m_alloc.set_throw_prob(0);

        const key_type k =
	  test_traits::generate_key(m_g, m_m);

        m_alloc.set_throw_prob(m_tp);

        typename cntnr::iterator found_it = m_p_c->find(k);

        typename native_type::iterator native_it = m_native_c.find(
								   test_traits::native_key(k));

        typename cntnr::const_reverse_iterator next_it = found_it;
        if (next_it != m_p_c->end())
	  ++next_it;

        typename cntnr::reverse_iterator next_ers_it =
	  m_p_c->erase((typename cntnr::reverse_iterator)found_it);

        PB_DS_THROW_IF_FAILED(            next_ers_it == next_it, "", m_p_c, & m_native_c);

        if (native_it != m_native_c.end())
	  m_native_c.erase(native_it);
      }
    catch(__gnu_cxx::forced_exception_error& )
      {
        done = false;

        PB_DS_THROW_IF_FAILED(            container_traits::erase_can_throw, container_traits::erase_can_throw, m_p_c, & m_native_c);
      }

  PB_DS_COND_COMPARE(*m_p_c, m_native_c);

  PB_DS_CANCEL_DESTRUCT_PRINT

    return (done);
}

