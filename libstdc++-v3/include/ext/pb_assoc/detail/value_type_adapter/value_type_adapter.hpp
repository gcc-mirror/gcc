// -*- C++ -*-

// Copyright (C) 2005, 2006 Free Software Foundation, Inc.
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
 * @file value_type_adapter.hpp
 * Contains an adapter of mapping levels.
 */

#ifndef VALUE_TYPE_ADAPTER_HPP
#define VALUE_TYPE_ADAPTER_HPP

#include <ext/pb_assoc/detail/value_type_adapter/ref_pair.hpp>
#include <ext/pb_assoc/detail/assoc_cntnr_base.hpp>
#include <ext/pb_assoc/detail/value_type_adapter/invalidation_guarantee_selector.hpp>
#include <ext/pb_assoc/detail/type_utils.hpp>
#include <utility>
#include <algorithm>
#include <tr1/type_traits>  // for aligned_storage/alignment_of

namespace pb_assoc
{

  namespace detail
  {

#define PB_ASSOC_STATIC_ASSERT(UNIQUE, E) \
	typedef \
		pb_assoc::detail::static_assert_dummy_class< \
			sizeof(pb_assoc::detail::static_assert<(bool)(E)>)> \
			UNIQUE##static_assert_type

#define PB_ASSOC_CLASS_T_DEC \
	template< \
		typename Key, \
		typename Data, \
		class DS_Tag, \
		class Policy_Tl, \
		class Allocator, \
		int Mapping_Level>

#define PB_ASSOC_CLASS_C_DEC \
	value_type_adapter< \
		Key, \
		Data, \
		DS_Tag, \
		Policy_Tl, \
		Allocator, \
		Mapping_Level>

#define PB_ASSOC_BASE_C_DEC \
	cond_type< \
		Mapping_Level != 1, \
		value_type_adapter< \
			Key, \
			Data, \
			DS_Tag, \
			Policy_Tl, \
			Allocator, \
			Mapping_Level - 1>, \
		typename assoc_cntnr_base< \
			Key, \
			Data, \
			DS_Tag, \
			Policy_Tl, \
			Allocator>::type>::type

    template<typename Key,
	     typename Data,
	     class DS_Tag,
	     class Policy_Tl,
	     class Allocator,
	     int Mapping_Level>
    struct value_type_adapter : public PB_ASSOC_BASE_C_DEC
    {

    private:
      typedef typename PB_ASSOC_BASE_C_DEC my_base;

      typedef typename my_base::data_type my_base_data_type;

      enum
	{
	  same_alloc_type =
	  is_same_type<
	  typename my_base::allocator::template rebind<
	  char>::other,
	  typename my_base_data_type::allocator::template rebind<
	  char>::other>::value
	};

      PB_ASSOC_STATIC_ASSERT(wrong_level, Mapping_Level > 0);

      PB_ASSOC_STATIC_ASSERT(must_be_same_alloc, same_alloc_type);

#include <ext/pb_assoc/detail/value_type_adapter/value_type_traits.hpp>
#include <ext/pb_assoc/detail/value_type_adapter/it_value_type_traits.hpp>

      typedef
      it_value_type_traits_<
	typename base_it_key_type<
	my_base,
	Mapping_Level == 1>::type,
	typename my_base_data_type::const_key_reference,
	typename cond_type<
	is_same_type<
	typename my_base_data_type::data_type,
	null_data_type>::value,
	null_data_type,
	typename my_base_data_type::data_reference>::type,
	typename my_base_data_type::reference,
	typename my_base::allocator>
      it_value_type_traits_t;

#include <ext/pb_assoc/detail/value_type_adapter/iterator.hpp>

      typedef
      value_type_traits_<
	typename my_base::key_type,
	typename my_base_data_type::key_type,
	typename my_base_data_type::data_type,
	typename my_base::allocator>
      value_type_traits_t;

      enum
	{
	  has_data =
	  !is_same_type<
	  typename my_base_data_type::data_type,
	  null_data_type>::value
	};

    public:

      typedef typename Allocator::size_type size_type;

      typedef typename Allocator::difference_type difference_type;

      typedef typename my_base::allocator allocator;

      typedef typename it_value_type_traits_t::key_type it_key_type;

      typedef
      std::pair<
	typename my_base::key_type,
	typename my_base_data_type::key_type>
      key_type;

      typedef
      typename allocator::template rebind<
	key_type>::other::reference
      key_reference;

      typedef
      typename allocator::template rebind<
	key_type>::other::const_reference
      const_key_reference;

      typedef
      typename allocator::template rebind<
	key_type>::other::pointer
      key_pointer;

      typedef
      typename allocator::template rebind<
	key_type>::other::const_pointer
      const_key_pointer;

      typedef typename my_base_data_type::data_type data_type;

      typedef
      typename allocator::template rebind<
	data_type>::other::reference
      data_reference;

      typedef
      typename allocator::template rebind<
	data_type>::other::const_reference
      const_data_reference;

      typedef
      typename allocator::template rebind<
	data_type>::other::pointer
      data_pointer;

      typedef
      typename allocator::template rebind<
	data_type>::other::const_pointer
      const_data_pointer;

      typedef typename value_type_traits_t::value_type value_type;

      typedef typename value_type_traits_t::reference reference;

      typedef typename value_type_traits_t::const_reference const_reference;

      typedef typename value_type_traits_t::pointer pointer;

      typedef typename value_type_traits_t::const_pointer const_pointer;

      typedef
      it_<
	typename my_base::const_find_iterator,
	typename my_base_data_type::const_find_iterator,
	has_data,
	true>
      const_find_iterator;

      typedef
      it_<
	typename my_base::find_iterator,
	typename my_base_data_type::find_iterator,
	has_data,
	false>
      find_iterator;

      typedef
      it_<
	typename my_base::const_iterator,
	typename my_base_data_type::const_iterator,
	has_data,
	true>
      const_iterator;

      typedef
      it_<
	typename my_base::iterator,
	typename my_base_data_type::iterator,
	has_data,
	false>
      iterator;

      enum
	{
	  mapping_level = mapping_level_imp<
	  typename my_base::given_data_type>::value -1
	};

      // Tmp Ami rebind

      typedef compound_ds_tag ds_category;

      typedef
      typename cond_type<
	mapping_level == 1,
	typename cond_type<
	has_data,
	data_enabled_ms_tag,
	basic_ms_tag>::type,
	compound_data_enabled_ms_tag>::type
      ms_category;

      typedef
      typename cond_type<
	Mapping_Level == 1,
	DS_Tag,
	compound_ds_tag>::type
      effective_base_ds_tag;

      typedef ds_traits< my_base_data_type> base_data_ds_traits;

      enum
	{
	  erase_can_throw =
	  base_data_ds_traits::erase_can_throw
	};

      enum
	{
	  order_preserving =
	  order_preserving_imp<
	  my_base,
	  effective_base_ds_tag>::value&& 
	  base_data_ds_traits::order_preserving
	};

      enum
	{
	  erase_iterators =
	  base_data_ds_traits::erase_iterators
	};

      typedef
      typename ig_sel<
	typename invalidation_guarantee_imp<
	my_base,
	effective_base_ds_tag>::type,
	typename ds_traits<
	my_base_data_type>::invalidation_guarantee>::type
      invalidation_guarantee;

      enum
	{
	  reverse_iteration =
	  reverse_iteration_imp<
	  my_base,
	  effective_base_ds_tag>::value&& 
	  base_data_ds_traits::reverse_iteration
	};

      enum
	{
	  split_join = false
	};

    protected:
      typedef typename my_base_data_type::data_pointer erase_imp_ret_t;

    private:
      inline const_key_reference
      extract_key_imp(const_reference r_val, int_to_type<true>)
      {
	return (r_val.first);
      }

      inline const_key_reference
      extract_key_imp(const_reference r_val, int_to_type<false>)
      {
	return (r_val);
      }

      inline it_key_type
      extract_key_imp(typename iterator::const_reference r_val, int_to_type<true>)
      {
	return (r_val.first);
      }

      inline it_key_type
      extract_key_imp(typename iterator::const_reference r_val, int_to_type<false>)
      {
	return (r_val);
      }

    public:

      inline size_type
      size() const
      {
	return (std::distance(begin(), end()));
      }

      inline size_type
      max_size() const
      {
	return (my_base::max_size());
      }

      inline bool
      empty() const
      {
	return (size() == 0);
      }

      inline static const_key_reference
      extract_key(const_reference r_val)
      {
	return (extract_key_imp(
				r_val,
				int_to_type<has_data>()));
      }

      inline it_key_type
      extract_key(typename iterator::const_reference r_val)
      {
	return (extract_key_imp(
				r_val,
				int_to_type<has_data>()));
      }

      inline std::pair<
	find_iterator,
	bool>
      insert(const_reference r_val)
      {
	typedef std::pair< typename my_base::find_iterator, bool> base_ins_ret;

	// Tmp Ami
      }

      inline data_reference
      operator[](const_key_reference r_key)
      {
	return (subscript_imp(r_key));
      }

      inline const_find_iterator
      find(const_key_reference r_key) const
      {
	typename my_base::const_find_iterator it = my_base::find(r_key.first);

	if (it == my_base::end())
	  return (end());

	typename my_base_data_type::const_find_iterator sec_it =
	  it->second.find(r_key.second);

	if (sec_it == it->second.end())
	  return (end());

	return (const_find_iterator(it, sec_it));
      }

      inline find_iterator
      find(const_key_reference r_key)
      {
	typename my_base::find_iterator it = my_base::find(r_key.first);

	if (it == my_base::end())
	  return (end());

	typename my_base_data_type::find_iterator sec_it =
	  it->second.find(r_key.second);

	if (sec_it == it->second.end())
	  return (end());

	return (find_iterator(it, my_base::end(), sec_it));
      }

      inline const_data_reference
      operator[](const_key_reference r_key) const
      {
	return (my_base::operator[](r_key.first).operator[](r_key.second));
      }

      inline size_type
      erase(const_key_reference r_key)
      {
	typename my_base::find_iterator it =
	  my_base::find(r_key.first);

	if (it == my_base::end())
	  return (0);

	if (it->second.find(r_key.second) == it->second.end())
	  return (0);

	it->second.erase(r_key.second);

	return (1);
      }

#include <ext/pb_assoc/detail/value_type_adapter/erase_if_pred.hpp>

      template<class Pred>
      inline size_type
      erase_if(Pred prd)
      {
	typename my_base::iterator it = my_base::begin();

	typename my_base::iterator end_it = my_base::end();

	size_type ersd = 0;

	// Tmp Ami check erase can throw

	while (it != end_it)
	  {
	    if (it->second.empty() == false)
	      {
		erase_if_pred<Pred> p(prd, it);

		ersd += it->second.erase_if(p);
	      }

	    ++it;
	  }

	return (ersd);
      }

      void
      clear()
      {
	typename my_base::iterator it = my_base::begin();

	typename my_base::iterator end_it = my_base::end();

	while (it != end_it)
	  it->second.clear();
      }

      inline const_iterator
      begin() const
      {
	typename my_base::const_iterator it = my_base::begin();

	while (it != my_base::end()&&  it->second.size() == 0)
	  ++it;

	if (it == my_base::end())
	  return (end());

	return (const_iterator(it, my_base::end(), it->second.begin()));
      }

      inline iterator
      begin()
      {
	typename my_base::iterator it = my_base::begin();

	while (it != my_base::end()&&  it->second.size() == 0)
	  ++it;

	if (it == my_base::end())
	  return (end());

	return (iterator(it, my_base::end(), it->second.begin()));
      }

      inline const_iterator
      end() const
      {
	return (const_iterator(my_base::end(), my_base::end()));
      }

      inline iterator
      end()
      {
	return (iterator(my_base::end(), my_base::end()));
      }

    protected:

      virtual
      ~value_type_adapter()
      { }

#define PB_ASSOC_CLASS_NAME value_type_adapter

#define PB_ASSOC_DIRECT_BASE_C_DEC PB_ASSOC_BASE_C_DEC

#define PB_ASSOC_DIRECT_BASE_CAST_C_DEC \
	typename PB_ASSOC_DIRECT_BASE_C_DEC

#include <ext/pb_assoc/detail/constructors_destructor_fn_imps.hpp>

#undef PB_ASSOC_CLASS_NAME

#undef PB_ASSOC_DIRECT_BASE_C_DEC

#undef PB_ASSOC_DIRECT_BASE_CAST_C_DEC

      data_reference
      subscript_imp(const_key_reference r_key)
      {
	return (my_base::subscript_imp(r_key.first)[r_key.second]);
      }

    private:
      value_type_adapter& 
      operator=(const value_type_adapter& r_other);
    };

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_BASE_C_DEC

  } // namespace detail

} // namespace pb_assoc

#endif // #ifndef VALUE_TYPE_ADAPTER_HPP

