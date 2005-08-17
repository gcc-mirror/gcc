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

/*
 * @file order_statistics_imp.hpp
 * Contains forward declarations for order_statistics_key
 */

#ifndef ORDER_STATISTICS_IMP_HPP
#define ORDER_STATISTICS_IMP_HPP

#define PB_ASSOC_CLASS_T_DEC \
	template<class Key, class Allocator>

#define PB_ASSOC_CLASS_C_DEC \
	order_statistics_key< \
		Key, \
		Allocator>

PB_ASSOC_CLASS_T_DEC
inline
PB_ASSOC_CLASS_C_DEC::
order_statistics_key(const_key_reference r_key) :
  m_key(r_key),
  m_rank(1)
{ }

PB_ASSOC_CLASS_T_DEC
inline
PB_ASSOC_CLASS_C_DEC::
operator typename PB_ASSOC_CLASS_C_DEC::key_reference()
{
  return (m_key);
}

PB_ASSOC_CLASS_T_DEC
inline
PB_ASSOC_CLASS_C_DEC::
operator typename PB_ASSOC_CLASS_C_DEC::key_type() const
{
  return (m_key);
}

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template<class Cmp_Fn, class Allocator>

#define PB_ASSOC_CLASS_C_DEC \
	order_statistics_key_cmp< \
		Cmp_Fn, \
		Allocator>

PB_ASSOC_CLASS_T_DEC
inline
PB_ASSOC_CLASS_C_DEC::
order_statistics_key_cmp()
{ }

PB_ASSOC_CLASS_T_DEC
inline
PB_ASSOC_CLASS_C_DEC::
order_statistics_key_cmp(const Cmp_Fn& r_cmp_fn) :
  Cmp_Fn(r_cmp_fn)
{ }

PB_ASSOC_CLASS_T_DEC
inline bool
PB_ASSOC_CLASS_C_DEC::
operator()(const_key_reference r_lhs_key, const_key_reference r_rhs_key) const
{
  return Cmp_Fn::operator()((key_type)r_lhs_key, (key_type)r_rhs_key);
}

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::cmp_fn& 
PB_ASSOC_CLASS_C_DEC::
get_cmp_fn()
{
  return (*this);
}

PB_ASSOC_CLASS_T_DEC
inline const typename PB_ASSOC_CLASS_C_DEC::cmp_fn& 
PB_ASSOC_CLASS_C_DEC::
get_cmp_fn() const
{
  return (*this);
}

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template<class Key, class Allocator>

#define PB_ASSOC_CLASS_C_DEC \
	order_statistics_node_updator< \
		Key, \
		Allocator>

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
operator()(const_key_pointer p_key, const_key_pointer p_l_child_key, const_key_pointer p_r_child_key)
{
  /*
   * The left rank is 0 if there is no left child,
   *	or the rank of the left child, otherwise.
   */
  const size_type l_rank =(p_l_child_key == NULL)? 0 : p_l_child_key->m_rank;

  /*
   * The right rank is 0 if there is no right child,
   *	or the rank of the right child, otherwise.
   */
  const size_type r_rank =(p_r_child_key == NULL)? 0 : p_r_child_key->m_rank;

  /*
   * The rand of the entry is the sumb of the ranks of its
   *	children + 1 (for itself).
   */
  p_key->m_rank = 1 + l_rank + r_rank;
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
swap(PB_ASSOC_CLASS_C_DEC& /*r_other*/)
{ }

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template<class Cntnr>

#define PB_ASSOC_CLASS_C_DEC \
	find_by_order< \
		Cntnr>

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::iterator
PB_ASSOC_CLASS_C_DEC::
operator()(Cntnr& r_c, size_type order) const
{
  return find(r_c, order);
}

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::const_iterator
PB_ASSOC_CLASS_C_DEC::
operator()(const Cntnr& r_c, size_type order) const
{
  return find(r_c, order);
}

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::const_iterator
PB_ASSOC_CLASS_C_DEC::
find(const Cntnr& r_c, size_type order)
{
  if (order > r_c.size())
    return (r_c.end());

  /*
   * Start at the top of the tree.
   */
  typename Cntnr::const_node_iterator it = r_c.node_begin();

  /*
   * Loop up to a leaf.
   */
  while (it != r_c.node_end())
    {
      typename Cntnr::const_node_iterator l_it = it.l_child();

      /*
       * The order of the element, o, is the rank of the left
       *	child (for the entry itself).
       */
      const size_type o = (l_it == r_c.node_end())?
	0 :(*l_it)->m_rank;

      /*
       * If the current order, o, is the order requested,
       *	the key has been found.
       */
      if (order == o)
	return (*it);
      /*
       * If the current order, o, is larger than the order requested,
       *	we should move to the left subtree.
       */
      else if (order < o)
	it = l_it;
      /*
       * Otherwise adujst the requested order and move to the right subtree.
       */
      else
	{
	  order -= o + 1;

	  it = it.r_child();
	}
    }

  return (r_c.end());
}

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::iterator
PB_ASSOC_CLASS_C_DEC::
find(Cntnr& r_c, size_type order)
{
  if (order > r_c.size())
    return (r_c.end());

  /*
   * Start at the top of the tree.
   */
  typename Cntnr::node_iterator it = r_c.node_begin();

  /*
   * Loop up to a leaf.
   */
  while (it != r_c.node_end())
    {
      typename Cntnr::node_iterator l_it = it.l_child();

      /*
       * The order of the element, o, is the rank of the left
       *	child (for the entry itself).
       */
      const size_type o = (l_it == r_c.node_end())?
	0 :
	r_c.extract_key(*(*l_it)).m_rank;

      /*
       * If the current order, o, is the order requested,
       *	the key has been found.
       */
      if (order == o)
	return (*it);
      /*
       * If the current order, o, is larger than the order requested,
       *	we should move to the left subtree.
       */
      else if (order < o)
	it = l_it;
      /*
       * Otherwise adujst the requested order and move to the right subtree.
       */
      else
	{
	  order -= o + 1;

	  it = it.r_child();
	}
    }

  return (r_c.end());
}

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template<class Cntnr>

#define PB_ASSOC_CLASS_C_DEC \
	order_by_key< \
		Cntnr>

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::size_type
PB_ASSOC_CLASS_C_DEC::
operator()(const Cntnr& r_c, const underlying_key_type& r_key) const
{
  /*
   * The logic here is similar to that in order_by_key.
   */

  typename Cntnr::const_node_iterator it = r_c.node_begin();

  size_type ord = 0;

  while (it != r_c.node_end())
    {
      typename Cntnr::const_node_iterator l_it = it.l_child();

      if (r_c.get_cmp_fn().get_cmp_fn()(
					r_key,
					r_c.extract_key(*(*it)).m_key))
	it = l_it;
      else if (r_c.get_cmp_fn().get_cmp_fn()(
					     r_c.extract_key(*(*it)).m_key,
					     r_key))
	{

	  ord += (l_it == r_c.node_end())?
	    1 :
	    1 + r_c.extract_key(*(*l_it)).m_rank;

	  it = it.r_child();
	}
      else
	{
	  ord += (l_it == r_c.node_end())?
	    0 :
	    r_c.extract_key(*(*l_it)).m_rank;

	  it = r_c.node_end();
	}
    }

  return (ord);
}

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template<class Cntnr, class Allocator>

#define PB_ASSOC_CLASS_C_DEC \
	order_statistics_key_verifier< \
		Cntnr, \
		Allocator>

template<class Cntnr, class Allocator = std::allocator<char> >
class order_statistics_key_verifier
{
public:
  typedef Cntnr map;

  typedef Allocator allocator;

  typedef typename allocator::size_type size_type;

  typedef
  typename allocator::template rebind<map>::other::const_reference
  const_map_reference;

public:
  bool
  operator()(const Cntnr& r_c) const;

private:
  typedef typename Cntnr::const_node_iterator const_node_iterator;

  typedef typename Cntnr::const_iterator cntnr_const_it;

  typedef std::pair<bool, size_type> stat;

private:
  static stat
  verify_imp(const_node_iterator it, const_node_iterator end_it)
  {
    if (it == end_it)
      return (std::make_pair(true, 0));

    const stat l_ret =
      verify_imp(it.l_child(), end_it);

    const stat r_ret =
      verify_imp(it.r_child(), end_it);

    if (!l_ret.first || !r_ret.first)
      return (std::make_pair(false, 0));

    if ((*it)->m_rank != 1 + l_ret.second + r_ret.second)
      return (std::make_pair(false, 0));

    return (std::make_pair(true, (*it)->m_rank));
  }
};

PB_ASSOC_CLASS_T_DEC
bool
PB_ASSOC_CLASS_C_DEC::
operator()(const Cntnr& r_c) const
{
  const stat top_stat =
    verify_imp(r_c.node_begin(), r_c.node_end());

  return (top_stat.first);
}

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#endif // #ifndef ORDER_STATISTICS_IMP_HPP
