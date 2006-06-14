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
 * @file get_set_load_fn_imps.hpp
 * Containsert a random regression test for a specific container type.
 */

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
get_set_load()
{
  typedef
    pb_ds::detail::integral_constant<int,
    test_traits::get_set_load>
    get_set_load_ind;

  get_set_load_imp(get_set_load_ind());
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
get_set_load_imp(pb_ds::detail::false_type)
{ }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
get_set_load_imp(pb_ds::detail::true_type)
{
  PB_DS_TRACE("get_set_load");

  PB_DS_SET_DESTRUCT_PRINT

    m_p_c->get_load();

  m_alloc.set_throw_prob(1);

  typename alloc_t::group_throw_prob_adjustor adjust(
						     m_p_c->size());

  const float min_load = static_cast<float>(0.05);
  const float max_load = static_cast<float>(0.9);

  const float new_load =
    static_cast<float>(
		       m_g.get_prob()*  (max_load - min_load) +
		       min_load);

  m_p_c->set_load(new_load);

  PB_DS_THROW_IF_FAILED(
			m_p_c->get_load() == new_load,
			"",
			m_p_c,
			& m_native_c);

  PB_DS_COND_COMPARE(*m_p_c, m_native_c);

  PB_DS_CANCEL_DESTRUCT_PRINT
    }

