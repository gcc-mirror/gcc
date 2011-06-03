// -*- C++ -*-

// Copyright (C) 2005, 2006, 2009, 2010, 2011 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

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
 * @file assoc_container.hpp
 * Contains associative containers.
 */

#ifndef PB_DS_ASSOC_CNTNR_HPP
#define PB_DS_ASSOC_CNTNR_HPP

#include <bits/c++config.h>
#include <ext/typelist.h>
#include <ext/pb_ds/tag_and_trait.hpp>
#include <ext/pb_ds/detail/standard_policies.hpp>
#include <ext/pb_ds/detail/container_base_dispatch.hpp>
#include <ext/pb_ds/detail/branch_policy/traits.hpp>

namespace __gnu_pbds
{
  /**
   *  @addtogroup pbds
   *  @{
   */

#define PB_DS_HASH_BASE \
  detail::container_base_dispatch<Key, Mapped, _Alloc, Tag, \
    typename __gnu_cxx::typelist::append< \
    typename __gnu_cxx::typelist::create4<Hash_Fn, Eq_Fn, Resize_Policy, \
    detail::integral_constant<int, Store_Hash> >::type, Policy_Tl>::type>::type

  /// An abstract basic hash-based associative container.
  template<typename Key,
	   typename Mapped,
	   typename Hash_Fn,
	   typename Eq_Fn,
	   typename Resize_Policy,
	   bool Store_Hash,
	   typename Tag,
	   typename Policy_Tl,
	   typename _Alloc>
  class basic_hash_table : public PB_DS_HASH_BASE
  {
  private:
    typedef typename PB_DS_HASH_BASE 		base_type;

  public:
    virtual
    ~basic_hash_table() { }

  protected:
#define PB_DS_CLASS_NAME basic_hash_table
#include <ext/pb_ds/detail/constructors_destructor_fn_imps.hpp>
#undef PB_DS_CLASS_NAME

  private:
    basic_hash_table&
    operator=(const base_type&);
  };

#undef PB_DS_HASH_BASE


#define PB_DS_CC_HASH_BASE \
  basic_hash_table<Key, Mapped,	Hash_Fn, Eq_Fn, Resize_Policy, Store_Hash, \
		   cc_hash_tag,	\
	  typename __gnu_cxx::typelist::create1<Comb_Hash_Fn>::type, _Alloc>

  /// A concrete collision-chaining hash-based associative container.
  template<typename Key,
	   typename Mapped,
	   typename Hash_Fn = typename detail::default_hash_fn<Key>::type,
	   typename Eq_Fn = typename detail::default_eq_fn<Key>::type,
	   typename Comb_Hash_Fn = detail::default_comb_hash_fn::type,
	   typename Resize_Policy = typename detail::default_resize_policy<Comb_Hash_Fn>::type,
	   bool Store_Hash = detail::default_store_hash,
	   typename _Alloc = std::allocator<char> >
  class cc_hash_table :  public PB_DS_CC_HASH_BASE
  {
  private:
    typedef PB_DS_CC_HASH_BASE 			base_type;

  public:
    typedef cc_hash_tag	       			container_category;
    typedef Hash_Fn 				hash_fn;
    typedef Eq_Fn 				eq_fn;
    typedef Resize_Policy 			resize_policy;
    typedef Comb_Hash_Fn 			comb_hash_fn;

    // Default constructor.
    cc_hash_table() { }

    // Constructor taking some policy objects. r_hash_fn will be
    // copied by the Hash_Fn object of the container object.
    cc_hash_table(const hash_fn& h)
    : base_type(h) { }

    // Constructor taking some policy objects. r_hash_fn will be
    // copied by the hash_fn object of the container object, and
    // r_eq_fn will be copied by the eq_fn object of the container
    // object.
    cc_hash_table(const hash_fn& h, const eq_fn& e)
    : base_type(h, e) { }

    // Constructor taking some policy objects. r_hash_fn will be
    // copied by the hash_fn object of the container object, r_eq_fn
    // will be copied by the eq_fn object of the container object, and
    // r_comb_hash_fn will be copied by the comb_hash_fn object of the
    // container object.
    cc_hash_table(const hash_fn& h, const eq_fn& e, const comb_hash_fn& ch)
    : base_type(h, e, ch) { }

    // Constructor taking some policy objects. r_hash_fn will be
    // copied by the hash_fn object of the container object, r_eq_fn
    // will be copied by the eq_fn object of the container object,
    // r_comb_hash_fn will be copied by the comb_hash_fn object of the
    // container object, and r_resize_policy will be copied by the
    // resize_policy object of the container object.
    cc_hash_table(const hash_fn& h, const eq_fn& e, const comb_hash_fn& ch,
		  const resize_policy& rp)
    : base_type(h, e, ch, rp) { }

    // Constructor taking __iterators to a range of value_types. The
    // value_types between first_it and last_it will be inserted into
    // the container object.
    template<typename It>
    cc_hash_table(It first, It last)
    { base_type::copy_from_range(first, last); }

    // Constructor taking __iterators to a range of value_types and
    // some policy objects. The value_types between first_it and
    // last_it will be inserted into the container object.
    template<typename It>
    cc_hash_table(It first, It last, const hash_fn& h)
    : base_type(h)
    { this->copy_from_range(first, last); }

    // Constructor taking __iterators to a range of value_types and
    // some policy objects The value_types between first_it and
    // last_it will be inserted into the container object. r_hash_fn
    // will be copied by the hash_fn object of the container object,
    // and r_eq_fn will be copied by the eq_fn object of the container
    // object.
    template<typename It>
    cc_hash_table(It first, It last, const hash_fn& h, const eq_fn& e)
    : base_type(h, e)
    { this->copy_from_range(first, last); }

    // Constructor taking __iterators to a range of value_types and
    // some policy objects The value_types between first_it and
    // last_it will be inserted into the container object. r_hash_fn
    // will be copied by the hash_fn object of the container object,
    // r_eq_fn will be copied by the eq_fn object of the container
    // object, and r_comb_hash_fn will be copied by the comb_hash_fn
    // object of the container object.
    template<typename It>
    cc_hash_table(It first, It last, const hash_fn& h, const eq_fn& e,
		  const comb_hash_fn& ch)
    : base_type(h, e, ch)
    { this->copy_from_range(first, last); }

    // Constructor taking __iterators to a range of value_types and
    // some policy objects The value_types between first_it and
    // last_it will be inserted into the container object. r_hash_fn
    // will be copied by the hash_fn object of the container object,
    // r_eq_fn will be copied by the eq_fn object of the container
    // object, r_comb_hash_fn will be copied by the comb_hash_fn
    // object of the container object, and r_resize_policy will be
    // copied by the resize_policy object of the container object.
    template<typename It>
    cc_hash_table(It first, It last, const hash_fn& h, const eq_fn& e,
		  const comb_hash_fn& ch, const resize_policy& rp)
    : base_type(h, e, ch, rp)
    { this->copy_from_range(first, last); }

    cc_hash_table(const cc_hash_table& other)
    : base_type((const base_type&)other)
    { }

    virtual
    ~cc_hash_table() { }

    cc_hash_table&
    operator=(const cc_hash_table& other)
    {
      if (this != &other)
	{
	  cc_hash_table tmp(other);
	  swap(tmp);
	}
      return *this;
    }

    void
    swap(cc_hash_table& other)
    { base_type::swap(other); }
  };

#undef PB_DS_CC_HASH_BASE


#define PB_DS_GP_HASH_BASE \
  basic_hash_table<Key, Mapped,	Hash_Fn, Eq_Fn, Resize_Policy, Store_Hash, \
		   gp_hash_tag, \
  typename __gnu_cxx::typelist::create2<Comb_Probe_Fn, Probe_Fn>::type, _Alloc>

  /// A concrete general-probing hash-based associative container.
  template<typename Key,
	   typename Mapped,
	   typename Hash_Fn = typename detail::default_hash_fn<Key>::type,
	   typename Eq_Fn = typename detail::default_eq_fn<Key>::type,
	   typename Comb_Probe_Fn = detail::default_comb_hash_fn::type,
	   typename Probe_Fn = typename detail::default_probe_fn<Comb_Probe_Fn>::type,
	   typename Resize_Policy = typename detail::default_resize_policy<Comb_Probe_Fn>::type,
	   bool Store_Hash = detail::default_store_hash,
	   typename _Alloc = std::allocator<char> >
  class gp_hash_table : public PB_DS_GP_HASH_BASE
  {
  private:
    typedef PB_DS_GP_HASH_BASE 			base_type;

  public:
    typedef gp_hash_tag	       			container_category;
    typedef Hash_Fn 				hash_fn;
    typedef Eq_Fn 				eq_fn;
    typedef Comb_Probe_Fn			comb_probe_fn;
    typedef Probe_Fn 				probe_fn;
    typedef Resize_Policy 			resize_policy;

    // Default constructor.
    gp_hash_table() { }

    // Constructor taking some policy objects. r_hash_fn will be
    // copied by the hash_fn object of the container object.
    gp_hash_table(const hash_fn& h)
    : base_type(h) { }

    // Constructor taking some policy objects. r_hash_fn will be
    // copied by the hash_fn object of the container object, and
    // r_eq_fn will be copied by the eq_fn object of the container
    // object.
    gp_hash_table(const hash_fn& h, const eq_fn& e)
    : base_type(h, e) { }

    // Constructor taking some policy objects. r_hash_fn will be
    // copied by the hash_fn object of the container object, r_eq_fn
    // will be copied by the eq_fn object of the container object, and
    // r_comb_probe_fn will be copied by the comb_probe_fn object of
    // the container object.
    gp_hash_table(const hash_fn& h, const eq_fn& e, const comb_probe_fn& cp)
    : base_type(h, e, cp) { }

    // Constructor taking some policy objects. r_hash_fn will be
    // copied by the hash_fn object of the container object, r_eq_fn
    // will be copied by the eq_fn object of the container object,
    // r_comb_probe_fn will be copied by the comb_probe_fn object of
    // the container object, and r_probe_fn will be copied by the
    // probe_fn object of the container object.
    gp_hash_table(const hash_fn& h, const eq_fn& e, const comb_probe_fn& cp,
		  const probe_fn& p)
    : base_type(h, e, cp, p) { }

    // Constructor taking some policy objects. r_hash_fn will be
    // copied by the hash_fn object of the container object, r_eq_fn
    // will be copied by the eq_fn object of the container object,
    // r_comb_probe_fn will be copied by the comb_probe_fn object of
    // the container object, r_probe_fn will be copied by the probe_fn
    // object of the container object, and r_resize_policy will be
    // copied by the Resize_Policy object of the container object.
    gp_hash_table(const hash_fn& h, const eq_fn& e, const comb_probe_fn& cp,
		  const probe_fn& p, const resize_policy& rp)
    : base_type(h, e, cp, p, rp) { }

    // Constructor taking __iterators to a range of value_types. The
    // value_types between first_it and last_it will be inserted into
    // the container object.
    template<typename It>
    gp_hash_table(It first, It last)
    { base_type::copy_from_range(first, last); }

    // Constructor taking __iterators to a range of value_types and
    // some policy objects. The value_types between first_it and
    // last_it will be inserted into the container object. r_hash_fn
    // will be copied by the hash_fn object of the container object.
    template<typename It>
    gp_hash_table(It first, It last, const hash_fn& h)
    : base_type(h)
    { base_type::copy_from_range(first, last); }

    // Constructor taking __iterators to a range of value_types and
    // some policy objects. The value_types between first_it and
    // last_it will be inserted into the container object. r_hash_fn
    // will be copied by the hash_fn object of the container object,
    // and r_eq_fn will be copied by the eq_fn object of the container
    // object.
    template<typename It>
    gp_hash_table(It first, It last, const hash_fn& h, const eq_fn& e)
    : base_type(h, e)
    { base_type::copy_from_range(first, last); }

    // Constructor taking __iterators to a range of value_types and
    // some policy objects. The value_types between first_it and
    // last_it will be inserted into the container object. r_hash_fn
    // will be copied by the hash_fn object of the container object,
    // r_eq_fn will be copied by the eq_fn object of the container
    // object, and r_comb_probe_fn will be copied by the comb_probe_fn
    // object of the container object.
    template<typename It>
    gp_hash_table(It first, It last, const hash_fn& h, const eq_fn& e,
		  const comb_probe_fn& cp)
    : base_type(h, e, cp)
    { base_type::copy_from_range(first, last); }

    // Constructor taking __iterators to a range of value_types and
    // some policy objects. The value_types between first_it and
    // last_it will be inserted into the container object. r_hash_fn
    // will be copied by the hash_fn object of the container object,
    // r_eq_fn will be copied by the eq_fn object of the container
    // object, r_comb_probe_fn will be copied by the comb_probe_fn
    // object of the container object, and r_probe_fn will be copied
    // by the probe_fn object of the container object.
    template<typename It>
    gp_hash_table(It first, It last, const hash_fn& h, const eq_fn& e,
		  const comb_probe_fn& cp, const probe_fn& p)
    : base_type(h, e, cp, p)
    { base_type::copy_from_range(first, last); }

    // Constructor taking __iterators to a range of value_types and
    // some policy objects. The value_types between first_it and
    // last_it will be inserted into the container object. r_hash_fn
    // will be copied by the hash_fn object of the container object,
    // r_eq_fn will be copied by the eq_fn object of the container
    // object, r_comb_probe_fn will be copied by the comb_probe_fn
    // object of the container object, r_probe_fn will be copied by
    // the probe_fn object of the container object, and
    // r_resize_policy will be copied by the resize_policy object of
    // the container object.
    template<typename It>
    gp_hash_table(It first, It last, const hash_fn& h, const eq_fn& e,
		  const comb_probe_fn& cp, const probe_fn& p,
		  const resize_policy& rp)
    : base_type(h, e, cp, p, rp)
    { base_type::copy_from_range(first, last); }

    gp_hash_table(const gp_hash_table& other)
    : base_type((const base_type&)other)
    { }

    virtual
    ~gp_hash_table() { }

    gp_hash_table&
    operator=(const gp_hash_table& other)
    {
      if (this != &other)
	{
	  gp_hash_table tmp(other);
	  swap(tmp);
	}
      return *this;
    }

    void
    swap(gp_hash_table& other)
    { base_type::swap(other); }
  };

#undef PB_DS_GP_HASH_BASE

#define PB_DS_BRANCH_BASE \
  detail::container_base_dispatch<Key, Mapped, _Alloc, Tag, Policy_Tl>::type

  /// An abstract basic tree-like (tree, trie) associative container.
  template<typename Key, typename Mapped, typename Tag,
	   typename Node_Update, typename Policy_Tl, typename _Alloc>
  class basic_branch : public PB_DS_BRANCH_BASE
  {
  private:
    typedef typename PB_DS_BRANCH_BASE 	       	base_type;

  public:
    typedef Node_Update 			node_update;

    virtual
    ~basic_branch() { }

  protected:
#define PB_DS_CLASS_NAME 		basic_branch
#include <ext/pb_ds/detail/constructors_destructor_fn_imps.hpp>
#undef PB_DS_CLASS_NAME
  };

#undef PB_DS_BRANCH_BASE


#define PB_DS_TREE_NODE_AND_IT_TRAITS \
  detail::tree_traits<Key, Mapped,Cmp_Fn,Node_Update,Tag,_Alloc>

#define PB_DS_TREE_BASE \
  basic_branch<Key,Mapped, Tag, \
	       typename PB_DS_TREE_NODE_AND_IT_TRAITS::node_update, \
	       typename __gnu_cxx::typelist::create2<Cmp_Fn, \
	       PB_DS_TREE_NODE_AND_IT_TRAITS>::type, _Alloc>

  /// A basic tree-based associative container.
  template<typename Key, typename Mapped, typename Cmp_Fn = std::less<Key>,
	   typename Tag = rb_tree_tag,
	   template<typename Node_CItr, typename Node_Itr,
		    typename Cmp_Fn_, typename _Alloc_>
	   class Node_Update = null_node_update,
	   typename _Alloc = std::allocator<char> >
  class tree : public PB_DS_TREE_BASE
  {
  private:
    typedef PB_DS_TREE_BASE 			base_type;

  public:
    // Comparison functor type.
    typedef Cmp_Fn 				cmp_fn;

    tree() { }

    // Constructor taking some policy objects. r_cmp_fn will be copied
    // by the Cmp_Fn object of the container object.
    tree(const cmp_fn& c)
    : base_type(c) { }

    // Constructor taking __iterators to a range of value_types. The
    // value_types between first_it and last_it will be inserted into
    // the container object.
    template<typename It>
    tree(It first, It last)
    { base_type::copy_from_range(first, last); }

    // Constructor taking __iterators to a range of value_types and
    // some policy objects The value_types between first_it and
    // last_it will be inserted into the container object. r_cmp_fn
    // will be copied by the cmp_fn object of the container object.
    template<typename It>
    tree(It first, It last, const cmp_fn& c)
      : base_type(c)
    { base_type::copy_from_range(first, last); }

    tree(const tree& other)
    : base_type((const base_type&)other) { }

    virtual
    ~tree() { }

    tree&
    operator=(const tree& other)
    {
      if (this != &other)
	{
	  tree tmp(other);
	  swap(tmp);
	}
      return *this;
    }

    void
    swap(tree& other)
    { base_type::swap(other); }
  };

#undef PB_DS_TREE_BASE
#undef PB_DS_TREE_NODE_AND_IT_TRAITS


#define PB_DS_TRIE_NODE_AND_IT_TRAITS \
  detail::trie_traits<Key,Mapped,_ATraits,Node_Update,Tag,_Alloc>

#define PB_DS_TRIE_BASE \
  basic_branch<Key,Mapped,Tag, \
	       typename PB_DS_TRIE_NODE_AND_IT_TRAITS::node_update, \
	       typename __gnu_cxx::typelist::create2<_ATraits, \
	       PB_DS_TRIE_NODE_AND_IT_TRAITS >::type, _Alloc>

  /// A basic trie-based associative container.
  template<typename Key,
	   typename Mapped,
	   typename _ATraits = \
		    typename detail::default_trie_access_traits<Key>::type,
	   typename Tag = pat_trie_tag,
	   template<typename Node_CItr,
		    typename Node_Itr,
		    typename _ATraits_,
		    typename _Alloc_>
	   class Node_Update = null_node_update,
	   typename _Alloc = std::allocator<char> >
  class trie : public PB_DS_TRIE_BASE
  {
  private:
    typedef PB_DS_TRIE_BASE			base_type;

  public:
    // Element access traits type.
    typedef _ATraits 				access_traits;

    trie() { }

    // Constructor taking some policy objects. r_access_traits will
    // be copied by the _ATraits object of the container
    // object.
    trie(const access_traits& t)
    : base_type(t) { }

    // Constructor taking __iterators to a range of value_types. The
    // value_types between first_it and last_it will be inserted into
    // the container object.
    template<typename It>
    trie(It first, It last)
    { base_type::copy_from_range(first, last); }

    // Constructor taking __iterators to a range of value_types and
    // some policy objects. The value_types between first_it and
    // last_it will be inserted into the container object.
    template<typename It>
    trie(It first, It last, const access_traits& t)
    : base_type(t)
    { base_type::copy_from_range(first, last); }

    trie(const trie& other)
    : base_type((const base_type&)other) { }

    virtual
    ~trie() { }

    trie&
    operator=(const trie& other)
    {
      if (this != &other)
	{
	  trie tmp(other);
	  swap(tmp);
	}
      return *this;
    }

    void
    swap(trie& other)
    { base_type::swap(other); }
  };

#undef PB_DS_TRIE_BASE
#undef PB_DS_TRIE_NODE_AND_IT_TRAITS


#define PB_DS_LU_BASE \
  detail::container_base_dispatch<Key, Mapped, _Alloc, list_update_tag,	\
    typename __gnu_cxx::typelist::create2<Eq_Fn, Update_Policy>::type>::type

  /// A list-update based associative container.
  template<typename Key,
	   typename Mapped,
	   class Eq_Fn = typename detail::default_eq_fn<Key>::type,
	   class Update_Policy = detail::default_update_policy::type,
	   class _Alloc = std::allocator<char> >
  class list_update : public PB_DS_LU_BASE
  {
  private:
    typedef typename PB_DS_LU_BASE 		base_type;

  public:
    typedef list_update_tag	       		container_category;
    typedef Eq_Fn 				eq_fn;
    typedef Update_Policy 			update_policy;

    list_update() { }

    // Constructor taking __iterators to a range of value_types. The
    // value_types between first_it and last_it will be inserted into
    // the container object.
    template<typename It>
    list_update(It first, It last)
    { base_type::copy_from_range(first, last); }

    list_update(const list_update& other)
    : base_type((const base_type&)other) { }

    virtual
    ~list_update() { }

    list_update&
    operator=(const list_update& other)
    {
      if (this !=& other)
	{
	  list_update tmp(other);
	  swap(tmp);
	}
      return *this;
    }

    void
    swap(list_update& other)
    { base_type::swap(other); }
  };

#undef PB_DS_LU_BASE

  // @} group pbds
} // namespace __gnu_pbds

#endif
