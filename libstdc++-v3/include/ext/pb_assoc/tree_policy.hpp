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
 * @file tree_policy.hpp
 * Contains tree-related policies.
 */

#ifndef TREE_POLICY_HPP
#define TREE_POLICY_HPP

#include <functional>
#include <ext/pb_assoc/ms_trait.hpp>

namespace pb_assoc
{
  struct null_node_updator
  {
    inline void
    swap(null_node_updator& r_other);
  };

#include <ext/pb_assoc/detail/tree_policy/null_node_updator_imp.hpp>

  template<typename Key, typename Allocator = std::allocator<char> >
    class order_statistics_key
    {
    public:
      typedef Allocator 			allocator;
      typedef Key 				key_type;
      typedef typename allocator::template rebind<Key>::other::const_reference
      						const_key_reference;
      typedef typename allocator::template rebind<Key>::other::reference
      						key_reference;
      typedef typename allocator::size_type 	size_type;
      
      inline explicit
      order_statistics_key(const_key_reference r_key = Key());
      
      inline
      operator key_reference();
      
      inline
      operator key_type() const;
      
    private:
      // The logical key of the entry.
      key_type m_key;
      
      // The number of entries in the subtree rooted at the node of
      // this element.
      mutable size_type m_rank;

      template<typename Cntnr>
        friend class order_by_key;
      
      template<typename Some_Cmp_Fn, typename Some_Allocator>
        friend class order_statistics_key_cmp;
      
      template<typename Some_Key, typename Some_Allocator>
        friend class order_statistics_node_updator;
      
      template<typename Cntnr>
        friend class find_by_order;
      
      template<typename Cntnr, typename Some_Allocator>
        friend class order_statistics_key_verifier;
  };

  template<typename Cmp_Fn, typename Allocator = std::allocator<char> >
    class order_statistics_key_cmp 
    : public std::binary_function<
      order_statistics_key<typename Cmp_Fn::first_argument_type, Allocator>,
      order_statistics_key<typename Cmp_Fn::second_argument_type, Allocator>, bool>, 
    private Cmp_Fn
    {
    public:
      typedef Allocator allocator;
      typedef Cmp_Fn cmp_fn;
      
      typedef 
      order_statistics_key<typename Cmp_Fn::first_argument_type, Allocator>
      key_type;
      
      typedef
      typename allocator::template rebind<key_type>::other::const_reference
      const_key_reference;
      
      inline
      order_statistics_key_cmp();
      
      inline
      order_statistics_key_cmp(const Cmp_Fn& r_cmp_fn);
      
      inline bool
      operator()(const_key_reference, const_key_reference) const;
      
      inline cmp_fn& 
      get_cmp_fn();
      
      inline const cmp_fn& 
      get_cmp_fn() const;
    };
  
#define PB_ASSOC_CLASS_C_DEC \
	order_statistics_node_updator<Key, Allocator>

  template<typename Key, typename Allocator = std::allocator<char> >
    class order_statistics_node_updator
    {
    public:
      typedef Allocator allocator;
      typedef order_statistics_key< Key, Allocator> key_type;

      typedef
      typename Allocator::template rebind<key_type>::other::const_pointer
      const_key_pointer;
      
      inline void
      swap(PB_ASSOC_CLASS_C_DEC& r_other);

      inline void
      operator()(const_key_pointer, const_key_pointer, const_key_pointer);

    private:
      typedef typename Allocator::size_type size_type;
    };

#undef PB_ASSOC_CLASS_C_DEC

  template<class Cntnr>
    class find_by_order
    {
    public:
      typedef Cntnr 				cntnr;
      typedef typename cntnr::iterator 		iterator;
      typedef typename cntnr::const_iterator	const_iterator;
      typedef typename cntnr::size_type 	size_type;
      
      inline iterator
      operator()(Cntnr& r_c, size_type order) const;
      
      inline const_iterator
      operator()(const Cntnr& r_c, size_type order) const;
      
    private:
      typedef typename Cntnr::node_iterator 	node_iterator;
      typedef typename Cntnr::const_iterator 	cntnr_const_it;
      typedef typename Cntnr::iterator 		cntnr_it;
      
      inline static iterator
      find(Cntnr& r_c, size_type order);
      
      inline static const_iterator
      find(const Cntnr& r_c, size_type order);
    };

  template<class Cntnr>
    class order_by_key
    {
    public:
      typedef Cntnr 				cntnr;
      typedef typename Cntnr::key_type 		order_statistics_key_type;
      typedef typename order_statistics_key_type::key_type
      						underlying_key_type;
      typedef typename cntnr::size_type 	size_type;
      
      inline size_type
      operator()(const Cntnr& r_c, const underlying_key_type& r_key) const;
      
    private:
      typedef typename cntnr::const_iterator cntnr_const_it;
      typedef typename cntnr::iterator cntnr_it;
    };
  
#include <ext/pb_assoc/detail/tree_policy/order_statistics_imp.hpp>
} // namespace pb_assoc

#endif // #ifndef TREE_POLICY_HPP
