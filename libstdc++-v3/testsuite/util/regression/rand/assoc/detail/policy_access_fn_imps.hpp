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
 * @file policy_access_fn_imps.hpp
 * Containsert a random regression test for a specific container type.
 */

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_access()
{
  policy_access(typename Cntnr::container_category());
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_access(pb_ds::basic_hash_tag)
{
  {
    typename Cntnr::hash_fn& r_t =
      m_p_c->get_hash_fn();

    assert(&r_t != NULL);
  }
  {
    const typename Cntnr::hash_fn& r_t =((const Cntnr& )*m_p_c).get_hash_fn();

    assert(&r_t != NULL);
  }

  {
    typename Cntnr::eq_fn& r_t =
      m_p_c->get_eq_fn();

    assert(&r_t != NULL);
  }
  {
    const typename Cntnr::eq_fn& r_t =((const Cntnr& )*m_p_c).get_eq_fn();

    assert(&r_t != NULL);
  }

  {
    typename Cntnr::resize_policy& r_t =
      m_p_c->get_resize_policy();

    assert(&r_t != NULL);
  }
  {
    const typename Cntnr::resize_policy& r_t =((const Cntnr& )*m_p_c).get_resize_policy();

    assert(&r_t != NULL);
  }
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_access(pb_ds::cc_hash_tag)
{
  policy_access(pb_ds::basic_hash_tag());

  {
    typename Cntnr::comb_hash_fn& r_t =
      m_p_c->get_comb_hash_fn();

    assert(&r_t != NULL);
  }
  {
    const typename Cntnr::comb_hash_fn& r_t =((const Cntnr& )*m_p_c).get_comb_hash_fn();

    assert(&r_t != NULL);
  }
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_access(pb_ds::gp_hash_tag)
{
  policy_access(pb_ds::basic_hash_tag());

  {
    typename Cntnr::comb_probe_fn& r_t =
      m_p_c->get_comb_probe_fn();

    assert(&r_t != NULL);
  }
  {
    const typename Cntnr::comb_probe_fn& r_t =((const Cntnr& )*m_p_c).get_comb_probe_fn();

    assert(&r_t != NULL);
  }

  {
    typename Cntnr::probe_fn& r_t =
      m_p_c->get_probe_fn();

    assert(&r_t != NULL);
  }
  {
    const typename Cntnr::probe_fn& r_t =((const Cntnr& )*m_p_c).get_probe_fn();

    assert(&r_t != NULL);
  }
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_access(pb_ds::tree_tag)
{
  {
    typename Cntnr::cmp_fn& r_t =
      m_p_c->get_cmp_fn();

    assert(&r_t != NULL);
  }

  {
    const typename Cntnr::cmp_fn& r_t =((const Cntnr& )*m_p_c).get_cmp_fn();

    assert(&r_t != NULL);
  }
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_access(pb_ds::list_update_tag)
{ }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_access(pb_ds::pat_trie_tag)
{
  typename Cntnr::e_access_traits& r_t =
    m_p_c->get_e_access_traits();

  assert(&r_t != NULL);
}

