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
 * @file assoc_cntnr.hpp
 * Contains associative containers.
 */

#ifndef ASSOC_CNTNR_HPP
#define ASSOC_CNTNR_HPP

#include <ext/pb_assoc/ms_trait.hpp>
#include <ext/pb_assoc/ds_trait.hpp>
#include <ext/pb_assoc/detail/type_utils.hpp>
#include <ext/pb_assoc/detail/typelist.hpp>
#include <ext/pb_assoc/detail/standard_policies.hpp>
#include <ext/pb_assoc/detail/mapping_level_imp.hpp>
#include <ext/pb_assoc/detail/assoc_cntnr_base.hpp>
#include <ext/pb_assoc/detail/value_type_adapter/value_type_adapter.hpp>

namespace pb_assoc
{

#define PB_ASSOC_CLASS_T_DEC \
	template< \
		typename Key, \
		typename Data, \
		class DS_Tag, \
		class Policy_Tl, \
		class Allocator>

#define PB_ASSOC_CLASS_C_DEC \
	basic_assoc_cntnr< \
		Key, \
		Data, \
		DS_Tag, \
		Policy_Tl, \
		Allocator>

#define PB_ASSOC_BASE_C_DEC \
	detail::assoc_cntnr_base< \
		Key, \
		Data, \
		DS_Tag, \
		Policy_Tl, \
		Allocator>::type

#define PB_ASSOC_DIRECT_BASE_C_DEC \
	detail::assoc_cntnr_base< \
		Key, \
		Data, \
		DS_Tag, \
		Policy_Tl, \
		Allocator>::type

  template<typename Key,
	   typename Data,
	   class DS_Tag,
	   class Policy_Tl,
	   class Allocator>
  class basic_assoc_cntnr 
  : public detail::assoc_cntnr_base<Key, Data, DS_Tag, Policy_Tl, Allocator>::type
  {
  public:
    typedef typename Allocator::size_type size_type;
    typedef typename Allocator::difference_type difference_type;
    typedef DS_Tag ds_category;
    typedef data_enabled_ms_tag ms_category;
    typedef Allocator allocator;

    typedef
    typename allocator::template rebind<
      Key>::other::value_type
    key_type;

    typedef
    typename allocator::template rebind<
      Key>::other::reference
    key_reference;

    typedef
    typename allocator::template rebind<
      Key>::other::const_reference
    const_key_reference;

    typedef
    typename allocator::template rebind<
      Key>::other::pointer
    key_pointer;

    typedef
    typename allocator::template rebind<
      Key>::other::const_pointer
    const_key_pointer;

    typedef
    typename allocator::template rebind<
      Data>::other::value_type
    data_type;

    typedef
    typename allocator::template rebind<
      Data>::other::reference
    data_reference;

    typedef
    typename allocator::template rebind<
      Data>::other::const_reference
    const_data_reference;

    typedef
    typename allocator::template rebind<
      Data>::other::pointer
    data_pointer;

    typedef
    typename allocator::template rebind<
      Data>::other::const_pointer
    const_data_pointer;

    typedef
    typename allocator::template rebind<
      std::pair<const key_type, data_type> >::other::value_type
    value_type;

    typedef
    typename allocator::template rebind<
      std::pair<const key_type, data_type> >::other::reference
    reference;

    typedef
    typename allocator::template rebind<
      std::pair<const key_type, data_type> >::other::const_reference
    const_reference;

    typedef
    typename allocator::template rebind<
      std::pair<const key_type, data_type> >::other::pointer
    pointer;

    typedef
    typename allocator::template rebind<
      std::pair<const key_type, data_type> >::other::const_pointer
    const_pointer;

    typedef
    typename PB_ASSOC_BASE_C_DEC::const_find_iterator
    const_find_iterator;

    typedef typename PB_ASSOC_BASE_C_DEC::find_iterator find_iterator;

    typedef typename PB_ASSOC_BASE_C_DEC::const_iterator const_iterator;

    typedef typename PB_ASSOC_BASE_C_DEC::iterator iterator;

  public:

    virtual
    ~basic_assoc_cntnr();

    inline size_type
    size() const;

    inline size_type
    max_size() const;

    inline bool
    empty() const;

    inline static const_key_reference
    extract_key(const_reference r_val);

    inline std::pair<find_iterator, bool>
    insert(const_reference r_val);

    inline data_reference
    operator[](const_key_reference r_key);

    inline find_iterator
    find(const_key_reference r_key)
    {
      return (my_base::find(r_key));
    }

    inline const_find_iterator
    find(const_key_reference r_key) const
    {
      return (my_base::find(r_key));
    }

    inline const_data_reference
    operator[](const_key_reference r_key) const;

    inline size_type
    erase(const_key_reference r_key);

    template<class Pred>
    inline size_type
    erase_if(Pred prd);

    void
    clear();

    inline iterator
    begin();

    inline const_iterator
    begin() const;

    inline iterator
    end();

    inline const_iterator
    end() const;

  protected:

#define PB_ASSOC_CLASS_NAME basic_assoc_cntnr

#define PB_ASSOC_DIRECT_BASE_CAST_C_DEC \
	typename PB_ASSOC_DIRECT_BASE_C_DEC

#include <ext/pb_assoc/detail/constructors_destructor_fn_imps.hpp>

#undef PB_ASSOC_DIRECT_BASE_CAST_C_DEC

#undef PB_ASSOC_CLASS_NAME

  private:
    typedef typename PB_ASSOC_BASE_C_DEC my_base;

  private:
    basic_assoc_cntnr& 
    operator=(const PB_ASSOC_CLASS_C_DEC& r_other);
  };

#include <ext/pb_assoc/detail/basic_assoc_cntnr/constructors_destructor_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/iterators_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/info_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/erase_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/insert_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/d_insert_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/d_find_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/d_extract_key.hpp>

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_BASE_C_DEC

#undef PB_ASSOC_DIRECT_BASE_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template< \
		typename Key, \
		class Cntnr, \
		class DS_Tag, \
		class Policy_Tl, \
		class Allocator>

#define PB_ASSOC_CLASS_C_DEC \
	basic_assoc_cntnr< \
		Key, \
		compound_data_type< \
			Cntnr>, \
		DS_Tag, \
		Policy_Tl, \
		Allocator>

#define PB_ASSOC_DIRECT_BASE_C_DEC \
	detail::value_type_adapter< \
		Key, \
		compound_data_type< \
			Cntnr>, \
		DS_Tag, \
		Policy_Tl, \
		Allocator, \
		detail::mapping_level_imp< \
			compound_data_type< \
				Cntnr> >::value - 1>

#define PB_ASSOC_BASE_C_DEC \
	detail::assoc_cntnr_base< \
		Key, \
		compound_data_type< \
			Cntnr>, \
		DS_Tag, \
		Policy_Tl, \
		Allocator>::type

  template<typename Key,
	   class Cntnr,
	   class DS_Tag,
	   class Policy_Tl,
	   class Allocator>
  class basic_assoc_cntnr<Key, compound_data_type< Cntnr>, DS_Tag, Policy_Tl, Allocator> : public PB_ASSOC_DIRECT_BASE_C_DEC
  {
  public:
    typedef typename Allocator::size_type size_type;
    typedef typename Allocator::difference_type difference_type;
    typedef DS_Tag ds_category;
    typedef compound_data_enabled_ms_tag ms_category;
    typedef Allocator allocator;

    typedef
    typename allocator::template rebind<
      Key>::other::value_type
    key_type;

    typedef
    typename allocator::template rebind<
      Key>::other::reference
    key_reference;

    typedef
    typename allocator::template rebind<
      Key>::other::const_reference
    const_key_reference;

    typedef
    typename allocator::template rebind<
      Key>::other::pointer
    key_pointer;

    typedef
    typename allocator::template rebind<
      Key>::other::const_pointer
    const_key_pointer;

    typedef
    typename allocator::template rebind<
      Cntnr>::other::value_type
    data_type;

    typedef
    typename allocator::template rebind<
      Cntnr>::other::reference
    data_reference;

    typedef
    typename allocator::template rebind<
      Cntnr>::other::const_reference
    const_data_reference;

    typedef
    typename allocator::template rebind<
      Cntnr>::other::pointer
    data_pointer;

    typedef
    typename allocator::template rebind<
      Cntnr>::other::const_pointer
    const_data_pointer;

    typedef
    typename allocator::template rebind<
      std::pair<const key_type, Cntnr> >::other::value_type
    value_type;

    typedef
    typename allocator::template rebind<
      std::pair<const key_type, Cntnr> >::other::reference
    reference;

    typedef
    typename allocator::template rebind<
      std::pair<const key_type, Cntnr> >::other::const_reference
    const_reference;

    typedef
    typename allocator::template rebind<
      std::pair<const key_type, Cntnr> >::other::pointer
    pointer;

    typedef
    typename allocator::template rebind<
      std::pair<const key_type, Cntnr> >::other::const_pointer
    const_pointer;

    typedef
    typename PB_ASSOC_BASE_C_DEC::const_find_iterator
    const_find_iterator;

    typedef typename PB_ASSOC_BASE_C_DEC::find_iterator find_iterator;

    typedef typename PB_ASSOC_BASE_C_DEC::const_iterator const_iterator;

    typedef typename PB_ASSOC_BASE_C_DEC::iterator iterator;

    template<int Mapping_Level>
    struct rebind
    {
    private:
      enum
	{
	  mapping_level =
	  detail::mapping_level_imp<compound_data_type<Cntnr> >::value
	};

    public:
      typedef
      detail::value_type_adapter<
	Key,
	compound_data_type<
	Cntnr>,
	DS_Tag,
	Policy_Tl,
	Allocator,
	mapping_level - Mapping_Level>
      other;
    };

  public:

    virtual
    ~basic_assoc_cntnr();

    inline size_type
    size() const;

    inline size_type
    max_size() const;

    inline bool
    empty() const;

    inline static const_key_reference
    extract_key(const_reference r_val);

    inline std::pair<find_iterator, bool>
    insert(const_reference r_val);

    inline data_reference
    operator[](const_key_reference r_key);

    inline find_iterator
    find(const_key_reference r_key)
    {
      return (my_base::find(r_key));
    }

    inline const_find_iterator
    find(const_key_reference r_key) const
    {
      return (my_base::find(r_key));
    }

    inline const_data_reference
    operator[](const_key_reference r_key) const;

    inline size_type
    erase(const_key_reference r_key);

    template<class Pred>
    inline size_type
    erase_if(Pred prd);

    void
    clear();

    inline iterator
    begin();

    inline const_iterator
    begin() const;

    inline iterator
    end();

    inline const_iterator
    end() const;

  protected:

#define PB_ASSOC_CLASS_NAME basic_assoc_cntnr

#define PB_ASSOC_DIRECT_BASE_CAST_C_DEC \
	typename PB_ASSOC_DIRECT_BASE_C_DEC

#include <ext/pb_assoc/detail/constructors_destructor_fn_imps.hpp>

#undef PB_ASSOC_DIRECT_BASE_CAST_C_DEC

#undef PB_ASSOC_CLASS_NAME

  private:
    typedef typename PB_ASSOC_BASE_C_DEC my_base;

  private:
    basic_assoc_cntnr& 
    operator=(const PB_ASSOC_CLASS_C_DEC& r_other);
  };

#include <ext/pb_assoc/detail/basic_assoc_cntnr/constructors_destructor_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/iterators_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/info_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/erase_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/insert_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/d_insert_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/d_find_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/d_extract_key.hpp>

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_BASE_C_DEC

#undef PB_ASSOC_DIRECT_BASE_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template<typename Key, class DS_Tag, class Policy_Tl, class Allocator>

#define PB_ASSOC_CLASS_C_DEC \
	basic_assoc_cntnr< \
		Key, \
		null_data_type, \
		DS_Tag, \
		Policy_Tl, \
		Allocator>

#define PB_ASSOC_BASE_C_DEC \
	detail::assoc_cntnr_base< \
		Key, \
		null_data_type, \
		DS_Tag, \
		Policy_Tl, \
		Allocator>::type

  template<typename Key, class DS_Tag, class Policy_Tl, class Allocator>
  class basic_assoc_cntnr<Key, null_data_type, DS_Tag, Policy_Tl, Allocator> : public PB_ASSOC_BASE_C_DEC
  {
  public:
    typedef typename Allocator::size_type size_type;
    typedef typename Allocator::difference_type difference_type;
    typedef DS_Tag ds_category;
    typedef basic_ms_tag ms_category;
    typedef Allocator allocator;

    typedef
    typename allocator::template rebind<
      Key>::other::value_type
    key_type;

    typedef
    typename allocator::template rebind<
      Key>::other::reference
    key_reference;

    typedef
    typename allocator::template rebind<
      Key>::other::const_reference
    const_key_reference;

    typedef
    typename allocator::template rebind<
      Key>::other::pointer
    key_pointer;

    typedef
    typename allocator::template rebind<
      Key>::other::const_pointer
    const_key_pointer;

    typedef
    typename allocator::template rebind<
      key_type>::other::value_type
    value_type;

    typedef
    typename allocator::template rebind<
      key_type>::other::const_reference
    reference;

    typedef
    typename allocator::template rebind<
      key_type>::other::const_reference
    const_reference;

    typedef
    typename allocator::template rebind<
      key_type>::other::const_pointer
    pointer;

    typedef
    typename allocator::template rebind<key_type>::other::const_pointer
    const_pointer;

    typedef
    typename PB_ASSOC_BASE_C_DEC::const_find_iterator
    const_find_iterator;

    typedef typename PB_ASSOC_BASE_C_DEC::find_iterator find_iterator;

    typedef typename PB_ASSOC_BASE_C_DEC::const_iterator const_iterator;

    typedef typename PB_ASSOC_BASE_C_DEC::iterator iterator;

  public:

    virtual
    ~basic_assoc_cntnr();

    inline size_type
    size() const;

    inline size_type
    max_size() const;

    inline bool
    empty() const;

    inline static const_key_reference
    extract_key(const_reference r_val);

    inline std::pair<find_iterator, bool>
    insert(const_reference r_val);

    inline find_iterator
    find(const_key_reference r_key)
    { return (my_base::find(r_key)); }

    inline const_find_iterator
    find(const_key_reference r_key) const
    { return (my_base::find(r_key)); }

    inline size_type
    erase(const_key_reference r_key);

    template<class Pred>
    inline size_type
    erase_if(Pred prd);

    void
    clear();

    inline iterator
    begin();

    inline const_iterator
    begin() const;

    inline iterator
    end();

    inline const_iterator
    end() const;

  protected:

#define PB_ASSOC_CLASS_NAME basic_assoc_cntnr

#define PB_ASSOC_DIRECT_BASE_C_DEC PB_ASSOC_BASE_C_DEC

#define PB_ASSOC_DIRECT_BASE_CAST_C_DEC \
	typename PB_ASSOC_DIRECT_BASE_C_DEC

#include <ext/pb_assoc/detail/constructors_destructor_fn_imps.hpp>

#undef PB_ASSOC_DIRECT_BASE_CAST_C_DEC

#undef PB_ASSOC_DIRECT_BASE_C_DEC

#undef PB_ASSOC_CLASS_NAME

  private:
    typedef typename PB_ASSOC_BASE_C_DEC my_base;

  private:
    basic_assoc_cntnr& 
    operator=(const PB_ASSOC_CLASS_C_DEC& r_other);
  };

#include <ext/pb_assoc/detail/basic_assoc_cntnr/constructors_destructor_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/iterators_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/info_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/erase_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/insert_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_assoc_cntnr/extract_key.hpp>

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_BASE_C_DEC

#define PB_ASSOC_TYPELIST_CHAIN1(X0) pb_assoc::detail::typelist_chain<X0, pb_assoc::detail::null_type>
#define PB_ASSOC_TYPELIST_CHAIN2(X0, X1) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN1(X1) >
#define PB_ASSOC_TYPELIST_CHAIN3(X0, X1, X2) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN2(X1, X2) >
#define PB_ASSOC_TYPELIST_CHAIN4(X0, X1, X2, X3) pb_assoc::detail::typelist_chain<X0,  PB_ASSOC_TYPELIST_CHAIN3(X1, X2, X3) >
#define PB_ASSOC_TYPELIST_CHAIN5(X0, X1, X2, X3, X4) pb_assoc::detail::typelist_chain<X0,  PB_ASSOC_TYPELIST_CHAIN4(X1, X2, X3, X4) >
#define PB_ASSOC_TYPELIST_CHAIN6(X0, X1, X2, X3, X4, X5) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN5(X1, X2, X3, X4, X5) >
#define PB_ASSOC_TYPELIST_CHAIN7(X0, X1, X2, X3, X4, X5, X6) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN6(X1, X2, X3, X4, X5, X6) >
#define PB_ASSOC_TYPELIST_CHAIN8(X0, X1, X2, X3, X4, X5, X6, X7) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN7(X1, X2, X3, X4, X5, X6, X7) >
#define PB_ASSOC_TYPELIST_CHAIN9(X0, X1, X2, X3, X4, X5, X6, X7, X8) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN8(X1, X2, X3, X4, X5, X6, X7, X8) >
#define PB_ASSOC_TYPELIST_CHAIN10(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN9(X1, X2, X3, X4, X5, X6, X7, X8, X9) >
#define PB_ASSOC_TYPELIST_CHAIN11(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN10(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10) >
#define PB_ASSOC_TYPELIST_CHAIN12(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN11(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11) >
#define PB_ASSOC_TYPELIST_CHAIN13(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN12(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12) >
#define PB_ASSOC_TYPELIST_CHAIN14(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN13(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13) >
#define PB_ASSOC_TYPELIST_CHAIN15(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14) pb_assoc::detail::typelist_chain<X0, PB_ASSOC_TYPELIST_CHAIN14(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14) >

#define PB_ASSOC_CLASS_T_DEC \
	template< \
		typename Key, \
		typename Data, \
		class Hash_Fn, \
		class Eq_Fn, \
		class Resize_Policy, \
		bool Store_Hash, \
		class DS_Tag, \
		class Policy_TL, \
		class Allocator>

#define PB_ASSOC_CLASS_C_DEC \
	basic_hash_assoc_cntnr< \
		Key, \
		Data, \
		Hash_Fn, \
		Eq_Fn, \
		Resize_Policy, \
		Store_Hash, \
		DS_Tag, \
		Policy_TL, \
		Allocator>

#define PB_ASSOC_BASE_C_DEC \
	basic_assoc_cntnr< \
		Key, \
		Data, \
		DS_Tag, \
		typename pb_assoc::detail::typelist_append< \
			pb_assoc::detail::typelist< \
				PB_ASSOC_TYPELIST_CHAIN4( Hash_Fn, Eq_Fn, Resize_Policy, pb_assoc::detail::int_to_type<Store_Hash>) >, \
				Policy_TL>::type, \
		Allocator>

  template<typename Key,
	   typename Data,
	   class Hash_Fn,
	   class Eq_Fn,
	   class Resize_Policy,
	   bool Store_Hash,
	   class DS_Tag,
	   class Policy_TL,
	   class Allocator>
  class basic_hash_assoc_cntnr : public PB_ASSOC_BASE_C_DEC
  {
  public:
    typedef Hash_Fn hash_fn;

    typedef Eq_Fn eq_fn;

    typedef Resize_Policy resize_policy;

    enum
      {
	store_hash = Store_Hash
      };

    virtual
    ~basic_hash_assoc_cntnr();

    hash_fn& 
    get_hash_fn();

    const hash_fn& 
    get_hash_fn() const;

    eq_fn& 
    get_eq_fn();

    const eq_fn& 
    get_eq_fn() const;

    Resize_Policy& 
    get_resize_policy();

    const resize_policy& 
    get_resize_policy() const;

  protected:

    typedef typename Allocator::size_type size_type;

    virtual void
    do_resize(size_type new_size);

#define PB_ASSOC_CLASS_NAME basic_hash_assoc_cntnr

#define PB_ASSOC_DIRECT_BASE_C_DEC PB_ASSOC_BASE_C_DEC

#define PB_ASSOC_DIRECT_BASE_CAST_C_DEC \
	PB_ASSOC_DIRECT_BASE_C_DEC

#include <ext/pb_assoc/detail/constructors_destructor_fn_imps.hpp>

#undef PB_ASSOC_DIRECT_BASE_C_DEC

#undef PB_ASSOC_CLASS_NAME

#undef PB_ASSOC_DIRECT_BASE_C_DEC

  private:
    typedef PB_ASSOC_BASE_C_DEC my_base;

    basic_hash_assoc_cntnr& 
    operator=(const PB_ASSOC_CLASS_C_DEC& r_other);
  };

#include <ext/pb_assoc/detail/basic_hash_assoc_cntnr/constructors_destructor_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_hash_assoc_cntnr/resize_fn_imps.hpp>

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_BASE_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template< \
		typename Key, \
		typename Data, \
		class Hash_Fn, \
		class Eq_Fn, \
		class Comb_Hash_Fn, \
		class Resize_Policy, \
		bool Store_Hash, \
		class Allocator>

#define PB_ASSOC_CLASS_C_DEC \
	cc_hash_assoc_cntnr< \
		Key, \
		Data, \
		Hash_Fn, \
		Eq_Fn, \
		Comb_Hash_Fn, \
		Resize_Policy, \
		Store_Hash, \
		Allocator>

#define PB_ASSOC_BASE_C_DEC \
	basic_hash_assoc_cntnr< \
		Key, \
		Data, \
		Hash_Fn, \
		Eq_Fn, \
		Resize_Policy, \
		Store_Hash, \
		cc_hash_ds_tag, \
		pb_assoc::detail::typelist< \
			PB_ASSOC_TYPELIST_CHAIN1( \
				Comb_Hash_Fn) >, \
		Allocator>

  template<typename Key,
	   typename Data,
	   class Hash_Fn = typename pb_assoc::detail::def_hash_fn<Key>::type,
	   class Eq_Fn = typename pb_assoc::detail::def_eq_fn<Key>::type,
	   class Comb_Hash_Fn = pb_assoc::detail::def_comb_hash_fn::type,
	   class Resize_Policy =
	   typename pb_assoc::detail::def_resize_policy<Comb_Hash_Fn>::type,
	   bool Store_Hash = pb_assoc::detail::def_store_hash,
	   class Allocator = std::allocator<char> >
  class cc_hash_assoc_cntnr : public PB_ASSOC_BASE_C_DEC
  {
  public:
    typedef Comb_Hash_Fn comb_hash_fn;

    cc_hash_assoc_cntnr();

    cc_hash_assoc_cntnr(const Hash_Fn& r_hash_fn);

    cc_hash_assoc_cntnr(const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn);

    cc_hash_assoc_cntnr(const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, 
			const Comb_Hash_Fn& r_comb_hash_fn);

    cc_hash_assoc_cntnr(const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, 
			const Comb_Hash_Fn& r_comb_hash_fn, 
			const Resize_Policy& r_resize_policy);

    template<class It>
    cc_hash_assoc_cntnr(It first_it, It last_it);

    template<class It>
    cc_hash_assoc_cntnr(It first_it, It last_it, const Hash_Fn& r_hash_fn);

    template<class It>
    cc_hash_assoc_cntnr(It first_it, It last_it, const Hash_Fn& r_hash_fn, 
			const Eq_Fn& r_eq_fn);

    template<class It>
    cc_hash_assoc_cntnr(It first_it, It last_it, const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, const Comb_Hash_Fn& r_comb_hash_fn);

    template<class It>
    cc_hash_assoc_cntnr(It first_it, It last_it, const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, const Comb_Hash_Fn& r_comb_hash_fn, const Resize_Policy& r_resize_policy);

    cc_hash_assoc_cntnr(const PB_ASSOC_CLASS_C_DEC& r_other);

    virtual
    ~cc_hash_assoc_cntnr();

    PB_ASSOC_CLASS_C_DEC& 
    operator=(const PB_ASSOC_CLASS_C_DEC& r_other);

    void
    swap(PB_ASSOC_CLASS_C_DEC& r_other);

    comb_hash_fn& 
    get_comb_hash_fn();

    const comb_hash_fn& 
    get_comb_hash_fn() const;

  private:
    typedef PB_ASSOC_BASE_C_DEC my_base;
  };

#include <ext/pb_assoc/detail/cc_hash_assoc_cntnr/constructor_destructor_fn_imps.hpp>

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_BASE_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template< \
		typename Key, \
		typename Data, \
		class Hash_Fn, \
		class Eq_Fn, \
		class Comb_Probe_Fn, \
		class Probe_Fn, \
		class Resize_Policy, \
		bool Store_Hash, \
		class Allocator>

#define PB_ASSOC_CLASS_C_DEC \
	gp_hash_assoc_cntnr< \
		Key, \
		Data, \
		Hash_Fn, \
		Eq_Fn, \
		Comb_Probe_Fn, \
		Probe_Fn, \
		Resize_Policy, \
		Store_Hash, \
		Allocator>

#define PB_ASSOC_BASE_C_DEC \
	basic_hash_assoc_cntnr< \
		Key, \
		Data, \
		Hash_Fn, \
		Eq_Fn, \
		Resize_Policy, \
		Store_Hash, \
		gp_hash_ds_tag, \
		pb_assoc::detail::typelist< \
			PB_ASSOC_TYPELIST_CHAIN2( Comb_Probe_Fn, Probe_Fn) >, \
		Allocator>

  template<typename Key,
	   typename Data,
	   class Hash_Fn = typename pb_assoc::detail::def_hash_fn<Key>::type,
	   class Eq_Fn = typename pb_assoc::detail::def_eq_fn<Key>::type,
	   class Comb_Probe_Fn = pb_assoc::detail::def_comb_hash_fn::type,
	   class Probe_Fn = typename detail::def_probe_fn<Comb_Probe_Fn>::type,
	   class Resize_Policy =
	   typename pb_assoc::detail::def_resize_policy<Comb_Probe_Fn>::type,

	   bool Store_Hash = pb_assoc::detail::def_store_hash,
	   class Allocator = std::allocator<char> >
  class gp_hash_assoc_cntnr : public PB_ASSOC_BASE_C_DEC
  {
  public:
    typedef Comb_Probe_Fn comb_probe_fn;
    typedef Probe_Fn probe_fn;

    gp_hash_assoc_cntnr();

    gp_hash_assoc_cntnr(const Hash_Fn& r_hash_fn);

    gp_hash_assoc_cntnr(const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn);

    gp_hash_assoc_cntnr(const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, 
			const Comb_Probe_Fn& r_comb_probe_fn);

    gp_hash_assoc_cntnr(const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, 
			const Comb_Probe_Fn& r_comb_probe_fn, 
			const Probe_Fn& r_probe_fn);

    gp_hash_assoc_cntnr(const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, 
			const Comb_Probe_Fn& r_comb_probe_fn, 
			const Probe_Fn& r_probe_fn, 
			const Resize_Policy& r_resize_policy);

    template<class It>
    gp_hash_assoc_cntnr(It first_it, It last_it);

    template<class It>
    gp_hash_assoc_cntnr(It first_it, It last_it, const Hash_Fn& r_hash_fn);

    template<class It>
    gp_hash_assoc_cntnr(It first_it, It last_it, const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn);

    template<class It>
    gp_hash_assoc_cntnr(It first_it, It last_it, const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, const Comb_Probe_Fn& r_comb_probe_fn);

    template<class It>
    gp_hash_assoc_cntnr(It first_it, It last_it, const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, const Comb_Probe_Fn& r_comb_probe_fn, const Probe_Fn& r_probe_fn);

    template<class It>
    gp_hash_assoc_cntnr(It first_it, It last_it, const Hash_Fn& r_hash_fn, const Eq_Fn& r_eq_fn, const Comb_Probe_Fn& r_comb_probe_fn, const Probe_Fn& r_probe_fn, const Resize_Policy& r_resize_policy);

    gp_hash_assoc_cntnr(const PB_ASSOC_CLASS_C_DEC& r_other);

    virtual
    ~gp_hash_assoc_cntnr();

    PB_ASSOC_CLASS_C_DEC& 
    operator=(const PB_ASSOC_CLASS_C_DEC& r_other);

    void
    swap(PB_ASSOC_CLASS_C_DEC& r_other);

    comb_probe_fn& 
    get_comb_probe_fn();

    const comb_probe_fn& 
    get_comb_probe_fn() const;

    probe_fn& 
    get_probe_fn();

    const probe_fn& 
    get_probe_fn() const;

  private:
    typedef PB_ASSOC_BASE_C_DEC my_base;
  };

#include <ext/pb_assoc/detail/gp_hash_assoc_cntnr/constructor_destructor_fn_imps.hpp>

#undef PB_ASSOC_CLASS_T_DEC

#undef  PB_ASSOC_CLASS_C_DEC

#undef  PB_ASSOC_BASE_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template< \
		typename Key, \
		typename Data, \
		class Cmp_Fn, \
		class DS_Tag, \
		class Node_Updator, \
		class Allocator>

#define PB_ASSOC_CLASS_C_DEC \
	basic_tree_assoc_cntnr< \
		Key, \
		Data, \
		Cmp_Fn, \
		DS_Tag, \
		Node_Updator, \
		Allocator>

#define PB_ASSOC_BASE_C_DEC \
	basic_assoc_cntnr< \
		Key, \
		Data, \
		DS_Tag, \
		pb_assoc::detail::typelist< \
			PB_ASSOC_TYPELIST_CHAIN2( Cmp_Fn, Node_Updator) >, \
		Allocator>

  template<typename Key,
	   typename Data,
	   class Cmp_Fn,
	   class DS_Tag,
	   class Node_Updator,
	   class Allocator>
  class basic_tree_assoc_cntnr : public PB_ASSOC_BASE_C_DEC
  {
  public:
    typedef typename Allocator::size_type size_type;

    typedef
    typename Allocator::template rebind<
      Key>::other::const_reference
    const_key_reference;

    typedef Cmp_Fn cmp_fn;

    typedef Node_Updator node_updator;

    typedef typename PB_ASSOC_BASE_C_DEC::const_iterator const_iterator;

    typedef typename PB_ASSOC_BASE_C_DEC::iterator iterator;

    typedef
    typename PB_ASSOC_BASE_C_DEC::const_reverse_iterator
    const_reverse_iterator;

    typedef
    typename PB_ASSOC_BASE_C_DEC::reverse_iterator
    reverse_iterator;

    typedef
    typename PB_ASSOC_BASE_C_DEC::const_node_iterator
    const_node_iterator;

    typedef typename PB_ASSOC_BASE_C_DEC::node_iterator node_iterator;

    virtual
    ~basic_tree_assoc_cntnr();

    cmp_fn& 
    get_cmp_fn();

    const cmp_fn& 
    get_cmp_fn() const;

    node_updator& 
    get_node_updator();

    const node_updator& 
    get_node_updator() const;

    inline size_type
    erase(const_key_reference r_key);

    inline iterator
    erase(iterator it);

    inline reverse_iterator
    erase(reverse_iterator it)
    {
      return (my_base::erase(it));
    }

    inline reverse_iterator
    rbegin()
    {
      return (my_base::rbegin());
    }

    inline const_reverse_iterator
    rbegin() const
    {
      return (my_base::rbegin());
    }

    inline reverse_iterator
    rend()
    {
      return (my_base::rend());
    }

    inline const_reverse_iterator
    rend() const
    {
      return (my_base::rend());
    }

    inline node_iterator
    node_begin()
    {
      return (my_base::node_begin());
    }

    inline const_node_iterator
    node_begin() const
    {
      return (my_base::node_begin());
    }

    inline node_iterator
    node_end()
    {
      return (my_base::node_end());
    }

    inline const_node_iterator
    node_end() const
    {
      return (my_base::node_end());
    }

    void
    join(PB_ASSOC_CLASS_C_DEC& r_other);

    inline void
    split(const_key_reference r_key, PB_ASSOC_CLASS_C_DEC& r_other);

  protected:

#define PB_ASSOC_CLASS_NAME basic_tree_assoc_cntnr

#define PB_ASSOC_DIRECT_BASE_C_DEC PB_ASSOC_BASE_C_DEC

#define PB_ASSOC_DIRECT_BASE_CAST_C_DEC \
	PB_ASSOC_DIRECT_BASE_C_DEC

#include <ext/pb_assoc/detail/constructors_destructor_fn_imps.hpp>

#undef PB_ASSOC_DIRECT_BASE_CAST_C_DEC

#undef PB_ASSOC_CLASS_NAME

#undef PB_ASSOC_DIRECT_BASE_C_DEC

  private:
    typedef PB_ASSOC_BASE_C_DEC my_base;
  };

#include <ext/pb_assoc/detail/basic_tree_assoc_cntnr/constructors_destructor_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_tree_assoc_cntnr/policy_access_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_tree_assoc_cntnr/range_iteration_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_tree_assoc_cntnr/r_range_iteration_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_tree_assoc_cntnr/node_iteration_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_tree_assoc_cntnr/split_join_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_tree_assoc_cntnr/erase_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_tree_assoc_cntnr/r_erase_fn_imps.hpp>

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_BASE_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template< \
		typename Key, \
		typename Data, \
		class Cmp_Fn, \
		class Node_Updator, \
		class Allocator>

#define PB_ASSOC_CLASS_C_DEC \
	basic_tree_assoc_cntnr< \
		Key, \
		Data, \
		Cmp_Fn, \
		ov_tree_ds_tag, \
		Node_Updator, \
		Allocator>

#define PB_ASSOC_BASE_C_DEC \
	basic_assoc_cntnr< \
		Key, \
		Data, \
		ov_tree_ds_tag, \
		pb_assoc::detail::typelist< \
			PB_ASSOC_TYPELIST_CHAIN2( Cmp_Fn, Node_Updator) >, \
		Allocator>

  template<typename Key,
	   typename Data,
	   typename Cmp_Fn,
	   typename Node_Updator,
	   typename Allocator>
  class basic_tree_assoc_cntnr<Key, Data, Cmp_Fn, ov_tree_ds_tag, Node_Updator, Allocator> 
  : public PB_ASSOC_BASE_C_DEC
  {
  public:
    typedef typename Allocator::size_type size_type;

    typedef
    typename Allocator::template rebind<
      Key>::other::const_reference
    const_key_reference;

    typedef Cmp_Fn cmp_fn;

    typedef Node_Updator node_updator;

    typedef typename PB_ASSOC_BASE_C_DEC::const_iterator const_iterator;

    typedef typename PB_ASSOC_BASE_C_DEC::iterator iterator;

    typedef
    typename PB_ASSOC_BASE_C_DEC::const_node_iterator
    const_node_iterator;

    typedef typename PB_ASSOC_BASE_C_DEC::node_iterator node_iterator;

    virtual
    ~basic_tree_assoc_cntnr();

    cmp_fn& 
    get_cmp_fn();

    const cmp_fn& 
    get_cmp_fn() const;

    node_updator& 
    get_node_updator();

    const node_updator& 
    get_node_updator() const;

    inline size_type
    erase(const_key_reference r_key);

    inline iterator
    erase(iterator it);

    inline node_iterator
    node_begin()
    {
      return (my_base::node_begin());
    }

    inline const_node_iterator
    node_begin() const
    {
      return (my_base::node_begin());
    }

    inline node_iterator
    node_end()
    {
      return (my_base::node_end());
    }

    inline const_node_iterator
    node_end() const
    {
      return (my_base::node_end());
    }

    void
    join(PB_ASSOC_CLASS_C_DEC& r_other);

    inline void
    split(const_key_reference r_key, PB_ASSOC_CLASS_C_DEC& r_other);

  protected:

#define PB_ASSOC_CLASS_NAME basic_tree_assoc_cntnr

#define PB_ASSOC_DIRECT_BASE_C_DEC PB_ASSOC_BASE_C_DEC

#define PB_ASSOC_DIRECT_BASE_CAST_C_DEC \
	PB_ASSOC_DIRECT_BASE_C_DEC

#include <ext/pb_assoc/detail/constructors_destructor_fn_imps.hpp>

#undef PB_ASSOC_DIRECT_BASE_CAST_C_DEC

#undef PB_ASSOC_CLASS_NAME

#undef PB_ASSOC_DIRECT_BASE_C_DEC

  private:
    typedef PB_ASSOC_BASE_C_DEC my_base;
  };

#include <ext/pb_assoc/detail/basic_tree_assoc_cntnr/constructors_destructor_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_tree_assoc_cntnr/policy_access_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_tree_assoc_cntnr/range_iteration_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_tree_assoc_cntnr/node_iteration_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_tree_assoc_cntnr/split_join_fn_imps.hpp>
#include <ext/pb_assoc/detail/basic_tree_assoc_cntnr/erase_fn_imps.hpp>

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_BASE_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template< \
		typename Key, \
		typename Data, \
		class Cmp_Fn, \
		class DS_Tag, \
		class Node_Updator, \
		class Allocator>

#define PB_ASSOC_CLASS_C_DEC \
	tree_assoc_cntnr< \
		Key, \
		Data, \
		Cmp_Fn, \
		DS_Tag, \
		Node_Updator, \
		Allocator>

#define PB_ASSOC_BASE_C_DEC \
	basic_tree_assoc_cntnr< \
		Key, \
		Data, \
		Cmp_Fn, \
		DS_Tag, \
		Node_Updator, \
		Allocator>

  template<typename Key, typename Data, class Cmp_Fn =	std::less<Key>,
	   typename DS_Tag = rb_tree_ds_tag,
	   typename Node_Updator = pb_assoc::detail::def_node_updator,
	   typename Allocator = std::allocator<char> >
  class tree_assoc_cntnr : public PB_ASSOC_BASE_C_DEC
  {
    typedef Cmp_Fn cmp_fn;
    typedef Node_Updator node_updator;

  public:
    tree_assoc_cntnr();
    tree_assoc_cntnr(const cmp_fn& r_cmp_fn);
    tree_assoc_cntnr(const cmp_fn& r_cmp_fn, const Node_Updator&r);
 
    template<typename It>
      tree_assoc_cntnr(It first_it, It last_it);

    template<typename It>
      tree_assoc_cntnr(It first_it, It last_it, const cmp_fn& r_cmp_fn);

    template<typename It>
      tree_assoc_cntnr(It, It, const cmp_fn&, const Node_Updator&);

    tree_assoc_cntnr(const PB_ASSOC_CLASS_C_DEC& r_other);

    virtual
    ~tree_assoc_cntnr();

    PB_ASSOC_CLASS_C_DEC& 
    operator=(const PB_ASSOC_CLASS_C_DEC& r_other);

    void
    swap(PB_ASSOC_CLASS_C_DEC& r_other);

  private:
    typedef PB_ASSOC_BASE_C_DEC my_base;
  };

#include <ext/pb_assoc/detail/tree_assoc_cntnr/constructor_destructor_fn_imps.hpp>

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_BASE_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template< \
		typename Key, \
		typename Data, \
		class Eq_Fn, \
		class Update_Policy, \
		class Allocator>

#define PB_ASSOC_CLASS_C_DEC \
	lu_assoc_cntnr< \
		Key, \
		Data, \
		Eq_Fn, \
		Update_Policy, \
		Allocator>

#define PB_ASSOC_BASE_C_DEC \
	basic_assoc_cntnr< \
		Key, \
		Data, \
		lu_ds_tag, \
		pb_assoc::detail::typelist< \
			PB_ASSOC_TYPELIST_CHAIN2( Eq_Fn, Update_Policy) >, \
		Allocator>

  template<typename Key,
	   typename Data,
	   typename Eq_Fn = typename pb_assoc::detail::def_eq_fn<Key>::type,
	   typename Update_Policy = pb_assoc::detail::def_update_policy::type,
	   typename Allocator = std::allocator<char> >
  class lu_assoc_cntnr : public PB_ASSOC_BASE_C_DEC
  {
  public:
    typedef Eq_Fn eq_fn;
    typedef Allocator allocator;
    typedef Update_Policy update_policy;
    
    lu_assoc_cntnr();    
    lu_assoc_cntnr(const Eq_Fn& r_eq_fn);    
    lu_assoc_cntnr(const Eq_Fn& r_eq_fn, const Update_Policy& r_update_policy);
    
    template<typename It>
      lu_assoc_cntnr(It first_it, It last_it);

    template<typename It>
      lu_assoc_cntnr(It first_it, It last_it, const Eq_Fn& r_eq_fn);

    template<typename It>
      lu_assoc_cntnr(It, It, const Eq_Fn&, const Update_Policy&);

    lu_assoc_cntnr(const PB_ASSOC_CLASS_C_DEC& r_other);

    virtual
    ~lu_assoc_cntnr();

    PB_ASSOC_CLASS_C_DEC& 
    operator=(const PB_ASSOC_CLASS_C_DEC& r_other);

    void
    swap(PB_ASSOC_CLASS_C_DEC& r_other);

    eq_fn& 
    get_eq_fn();

    const eq_fn& 
    get_eq_fn() const;

    update_policy& 
    get_update_policy();

    const update_policy& 
    get_update_policy() const;

  private:
    typedef PB_ASSOC_BASE_C_DEC my_base;
  };

#include <ext/pb_assoc/detail/lu_assoc_cntnr/constructor_destructor_fn_imps.hpp>
#include <ext/pb_assoc/detail/lu_assoc_cntnr/policy_access_fn_imps.hpp>

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_BASE_C_DEC

#undef PB_ASSOC_TYPELIST_CHAIN1
#undef PB_ASSOC_TYPELIST_CHAIN2
#undef PB_ASSOC_TYPELIST_CHAIN3
#undef PB_ASSOC_TYPELIST_CHAIN4
#undef PB_ASSOC_TYPELIST_CHAIN5
#undef PB_ASSOC_TYPELIST_CHAIN6
#undef PB_ASSOC_TYPELIST_CHAIN7
#undef PB_ASSOC_TYPELIST_CHAIN8
#undef PB_ASSOC_TYPELIST_CHAIN9
#undef PB_ASSOC_TYPELIST_CHAIN10
#undef PB_ASSOC_TYPELIST_CHAIN11
#undef PB_ASSOC_TYPELIST_CHAIN12
#undef PB_ASSOC_TYPELIST_CHAIN13
#undef PB_ASSOC_TYPELIST_CHAIN14
#undef PB_ASSOC_TYPELIST_CHAIN15

} // namespace pb_assoc

#endif // #ifndef ASSOC_CNTNR_HPP
