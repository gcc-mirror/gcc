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
 * Contains an implementation class for splay_tree_.
 */

PB_DS_CLASS_T_DEC
inline void
PB_DS_CLASS_C_DEC::
join(PB_DS_CLASS_C_DEC& other)
{
  PB_DS_DBG_ONLY(assert_valid();)
    PB_DS_DBG_ONLY(other.assert_valid();)

    if (PB_DS_BASE_C_DEC::join_prep(other) == false)
      {
        PB_DS_DBG_ONLY(assert_valid();)
	  PB_DS_DBG_ONLY(other.assert_valid();)

	  return;
      }

  node_pointer p_target_r = other.leftmost(other.m_p_head);

  PB_DS_DBG_ASSERT(p_target_r != NULL);

  other.splay(p_target_r);

  PB_DS_DBG_ASSERT(p_target_r == other.m_p_head->m_p_parent);
  PB_DS_DBG_ASSERT(p_target_r->m_p_left == NULL);

  p_target_r->m_p_left = PB_DS_BASE_C_DEC::m_p_head->m_p_parent;

  PB_DS_DBG_ASSERT(p_target_r->m_p_left != NULL);
  p_target_r->m_p_left->m_p_parent = p_target_r;

  PB_DS_BASE_C_DEC::m_p_head->m_p_parent = p_target_r;
  p_target_r->m_p_parent = PB_DS_BASE_C_DEC::m_p_head;

  apply_update(p_target_r, (node_update* )this);

  PB_DS_BASE_C_DEC::join_finish(other);

  PB_DS_DBG_ONLY(assert_valid();)
    PB_DS_DBG_ONLY(other.assert_valid();)
    }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
split(const_key_reference r_key, PB_DS_CLASS_C_DEC& other)
{
  PB_DS_DBG_ONLY(assert_valid());
  PB_DS_DBG_ONLY(other.assert_valid());

  if (PB_DS_BASE_C_DEC::split_prep(r_key, other) == false)
    {
      PB_DS_DBG_ONLY(assert_valid());
      PB_DS_DBG_ONLY(other.assert_valid());

      return;
    }

  node_pointer p_upper_bound = upper_bound(r_key).m_p_nd;
  PB_DS_DBG_ASSERT(p_upper_bound != NULL);

  splay(p_upper_bound);
  PB_DS_DBG_ASSERT(p_upper_bound->m_p_parent == this->m_p_head);

  node_pointer p_new_root = p_upper_bound->m_p_left;
  PB_DS_DBG_ASSERT(p_new_root != NULL);

  PB_DS_BASE_C_DEC::m_p_head->m_p_parent = p_new_root;
  p_new_root->m_p_parent = PB_DS_BASE_C_DEC::m_p_head;

  other.m_p_head->m_p_parent = p_upper_bound;
  p_upper_bound->m_p_parent = other.m_p_head;
  p_upper_bound->m_p_left = NULL;

  apply_update(p_upper_bound, (node_update* )this);

  PB_DS_BASE_C_DEC::split_finish(other);

  PB_DS_DBG_ONLY(assert_valid());
  PB_DS_DBG_ONLY(other.assert_valid());
}

