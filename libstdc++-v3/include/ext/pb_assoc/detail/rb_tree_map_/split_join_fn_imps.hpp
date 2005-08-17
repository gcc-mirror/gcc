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
 * @file split_join_fn_imps.hpp
 * Contains an implementation for rb_tree_.
 */

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
join(PB_ASSOC_CLASS_C_DEC& r_other)
{
  PB_ASSOC_DBG_ONLY(assert_valid();)
    PB_ASSOC_DBG_ONLY(PB_ASSOC_BASE_C_DEC::assert_valid(true, true);)

    PB_ASSOC_DBG_ONLY(r_other.assert_valid();)
    PB_ASSOC_DBG_ONLY(r_other.PB_ASSOC_BASE_C_DEC::assert_valid(true, true);)

    if (PB_ASSOC_BASE_C_DEC::join_prep(r_other) == false)
      {
	PB_ASSOC_DBG_ONLY(assert_valid();)
	  PB_ASSOC_DBG_ONLY(r_other.assert_valid();)

	  return;
      }

  const node_pointer p_x = r_other.split_min();

  join_imp(p_x, r_other.m_p_head->m_p_parent);

  PB_ASSOC_BASE_C_DEC::join_finish(r_other);

  PB_ASSOC_DBG_ONLY(assert_valid();)
    PB_ASSOC_DBG_ONLY(PB_ASSOC_BASE_C_DEC::assert_valid(true, true);)

    PB_ASSOC_DBG_ONLY(r_other.assert_valid();)
    PB_ASSOC_DBG_ONLY(r_other.PB_ASSOC_BASE_C_DEC::assert_valid(true, true);)
    }

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
join_imp(node_pointer p_x, node_pointer p_r)
{
  PB_ASSOC_DBG_ASSERT(p_x != NULL);

  if (p_r != NULL)
    p_r->m_red = false;

  const size_type h =
    black_height(PB_ASSOC_BASE_C_DEC::m_p_head->m_p_parent);
  const size_type other_h = black_height(p_r);

  node_pointer p_x_l;
  node_pointer p_x_r;

  std::pair<node_pointer, node_pointer> join_pos;

  const bool right_join = h >= other_h;

  if (right_join)
    {
      join_pos = find_join_pos_right(PB_ASSOC_BASE_C_DEC::m_p_head->m_p_parent, h, other_h);

      p_x_l = join_pos.first;
      p_x_r = p_r;
    }
  else
    {
      p_x_l = PB_ASSOC_BASE_C_DEC::m_p_head->m_p_parent;

      PB_ASSOC_BASE_C_DEC::m_p_head->m_p_parent = p_r;
      if (p_r != NULL)
	p_r->m_p_parent = PB_ASSOC_BASE_C_DEC::m_p_head;

      join_pos = find_join_pos_left(PB_ASSOC_BASE_C_DEC::m_p_head->m_p_parent, h, other_h);

      p_x_r = join_pos.first;
    }

  node_pointer p_parent = join_pos.second;

  if (p_parent == PB_ASSOC_BASE_C_DEC::m_p_head)
    {
      PB_ASSOC_BASE_C_DEC::m_p_head->m_p_parent = p_x;

      p_x->m_p_parent = PB_ASSOC_BASE_C_DEC::m_p_head;
    }
  else
    {
      p_x->m_p_parent = p_parent;

      if (right_join)
	p_x->m_p_parent->m_p_right = p_x;
      else
	p_x->m_p_parent->m_p_left = p_x;
    }

  p_x->m_p_left = p_x_l;
  if (p_x_l != NULL)
    p_x_l->m_p_parent = p_x;

  p_x->m_p_right = p_x_r;
  if (p_x_r != NULL)
    p_x_r->m_p_parent = p_x;

  p_x->m_red = true;

  PB_ASSOC_BASE_C_DEC::initialize_min_max();

  PB_ASSOC_DBG_ONLY(PB_ASSOC_BASE_C_DEC::assert_valid(true, false);)

    PB_ASSOC_BASE_C_DEC::update_to_top(p_x, (Node_Updator* )this);

  insert_fixup(p_x);

  PB_ASSOC_DBG_ONLY(assert_valid());
  PB_ASSOC_DBG_ONLY(PB_ASSOC_BASE_C_DEC::assert_valid(true, false);)
    }

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::node_pointer
PB_ASSOC_CLASS_C_DEC::
split_min()
{
  PB_ASSOC_DBG_ASSERT(PB_ASSOC_BASE_C_DEC::m_p_head->m_p_left !=
		      PB_ASSOC_BASE_C_DEC::m_p_head);

  node_pointer p_min = PB_ASSOC_BASE_C_DEC::m_p_head->m_p_left;

  remove_node(p_min);

  return (p_min);
}

PB_ASSOC_CLASS_T_DEC
std::pair<
  typename PB_ASSOC_CLASS_C_DEC::node_pointer,
  typename PB_ASSOC_CLASS_C_DEC::node_pointer>
PB_ASSOC_CLASS_C_DEC::
find_join_pos_right(node_pointer p_l, size_type h_l, size_type h_r)
{
  PB_ASSOC_DBG_ASSERT(h_l >= h_r);

  if (PB_ASSOC_BASE_C_DEC::m_p_head->m_p_parent == NULL)
    return (std::make_pair((node_pointer)NULL,
			   PB_ASSOC_BASE_C_DEC::m_p_head));

  node_pointer p_l_parent = PB_ASSOC_BASE_C_DEC::m_p_head;

  while (h_l > h_r)
    {
      if (p_l->m_red == false)
	{
	  PB_ASSOC_DBG_ASSERT(h_l > 0);

	  --h_l;
	}

      p_l_parent = p_l;

      p_l = p_l->m_p_right;
    }

  if (!is_effectively_black(p_l))
    {
      p_l_parent = p_l;

      p_l = p_l->m_p_right;
    }

  PB_ASSOC_DBG_ASSERT(is_effectively_black(p_l));
  PB_ASSOC_DBG_ASSERT(black_height(p_l) == h_r);
  PB_ASSOC_DBG_ASSERT(p_l == NULL || p_l->m_p_parent == p_l_parent);

  return (std::make_pair(p_l, p_l_parent));
}

PB_ASSOC_CLASS_T_DEC
std::pair<
  typename PB_ASSOC_CLASS_C_DEC::node_pointer,
  typename PB_ASSOC_CLASS_C_DEC::node_pointer>
PB_ASSOC_CLASS_C_DEC::
find_join_pos_left(node_pointer p_r, size_type h_l, size_type h_r)
{
  PB_ASSOC_DBG_ASSERT(h_r > h_l);

  if (PB_ASSOC_BASE_C_DEC::m_p_head->m_p_parent == NULL)
    return (std::make_pair((node_pointer)NULL,
			   PB_ASSOC_BASE_C_DEC::m_p_head));

  node_pointer p_r_parent = PB_ASSOC_BASE_C_DEC::m_p_head;

  while (h_r > h_l)
    {
      if (p_r->m_red == false)
	{
	  PB_ASSOC_DBG_ASSERT(h_r > 0);

	  --h_r;
	}

      p_r_parent = p_r;

      p_r = p_r->m_p_left;
    }

  if (!is_effectively_black(p_r))
    {
      p_r_parent = p_r;

      p_r = p_r->m_p_left;
    }

  PB_ASSOC_DBG_ASSERT(is_effectively_black(p_r));
  PB_ASSOC_DBG_ASSERT(black_height(p_r) == h_l);
  PB_ASSOC_DBG_ASSERT(p_r == NULL || p_r->m_p_parent == p_r_parent);

  return (std::make_pair(p_r, p_r_parent));
}

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::size_type
PB_ASSOC_CLASS_C_DEC::
black_height(node_pointer p_nd)
{
  size_type h = 1;

  while (p_nd != NULL)
    {
      if (p_nd->m_red == false)
	++h;

      p_nd = p_nd->m_p_left;
    }

  return (h);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
split(const_key_reference r_key, PB_ASSOC_CLASS_C_DEC& r_other)
{
  PB_ASSOC_DBG_ONLY(assert_valid());
  PB_ASSOC_DBG_ONLY(PB_ASSOC_BASE_C_DEC::assert_valid(true, true);)

    PB_ASSOC_DBG_ONLY(r_other.assert_valid());
  PB_ASSOC_DBG_ONLY(r_other.PB_ASSOC_BASE_C_DEC::assert_valid(true, true);)

    if (PB_ASSOC_BASE_C_DEC::split_prep(r_key, r_other) == false)
      {
	PB_ASSOC_DBG_ONLY(assert_valid());
	PB_ASSOC_DBG_ONLY(r_other.assert_valid());

	return;
      }

  PB_ASSOC_DBG_ONLY(assert_valid());
  PB_ASSOC_DBG_ONLY(PB_ASSOC_BASE_C_DEC::assert_valid(true, false);)

    PB_ASSOC_DBG_ONLY(r_other.assert_valid());
  PB_ASSOC_DBG_ONLY(r_other.PB_ASSOC_BASE_C_DEC::assert_valid(true, false);)

    node_pointer p_nd = upper_bound(r_key).m_p_nd;

  do
    {
      node_pointer p_next_nd = p_nd->m_p_parent;

      if (Cmp_Fn::operator()(
			     r_key,
			     PB_ASSOC_V2F(p_nd->m_value)))
	split_at_node(p_nd, r_other);

      PB_ASSOC_DBG_ONLY(assert_valid());
      PB_ASSOC_DBG_ONLY(PB_ASSOC_BASE_C_DEC::assert_valid(true, false);)

	PB_ASSOC_DBG_ONLY(r_other.assert_valid());
      PB_ASSOC_DBG_ONLY(r_other.PB_ASSOC_BASE_C_DEC::assert_valid(true, false);)

	p_nd = p_next_nd;
    }
  while (p_nd != PB_ASSOC_BASE_C_DEC::m_p_head);

  PB_ASSOC_BASE_C_DEC::split_finish(r_other);

  PB_ASSOC_DBG_ONLY(assert_valid());
  PB_ASSOC_DBG_ONLY(PB_ASSOC_BASE_C_DEC::assert_valid(true, true);)

    PB_ASSOC_DBG_ONLY(r_other.assert_valid());
  PB_ASSOC_DBG_ONLY(r_other.PB_ASSOC_BASE_C_DEC::assert_valid(true, true);)
    }

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
split_at_node(node_pointer p_nd, PB_ASSOC_CLASS_C_DEC& r_other)
{
  PB_ASSOC_DBG_ASSERT(p_nd != NULL);

  node_pointer p_l = p_nd->m_p_left;
  node_pointer p_r = p_nd->m_p_right;

  node_pointer p_parent = p_nd->m_p_parent;

  if (p_parent == PB_ASSOC_BASE_C_DEC::m_p_head)
    {
      PB_ASSOC_BASE_C_DEC::m_p_head->m_p_parent = p_l;

      if (p_l != NULL)
	{
	  p_l->m_p_parent = PB_ASSOC_BASE_C_DEC::m_p_head;

	  p_l->m_red = false;
	}
    }
  else
    {
      if (p_parent->m_p_left == p_nd)
	p_parent->m_p_left = p_l;
      else
	p_parent->m_p_right = p_l;

      if (p_l != NULL)
	p_l->m_p_parent = p_parent;

      update_to_top(p_parent, (Node_Updator* )this);

      if (!p_nd->m_red)
	remove_fixup(p_l, p_parent);
    }

  PB_ASSOC_BASE_C_DEC::initialize_min_max();

  PB_ASSOC_DBG_ONLY(assert_valid());
  PB_ASSOC_DBG_ONLY(PB_ASSOC_BASE_C_DEC::assert_valid(true, false);)

    PB_ASSOC_DBG_ONLY(r_other.assert_valid());
  PB_ASSOC_DBG_ONLY(r_other.PB_ASSOC_BASE_C_DEC::assert_valid(true, false);)

    r_other.join_imp(p_nd, p_r);
}

