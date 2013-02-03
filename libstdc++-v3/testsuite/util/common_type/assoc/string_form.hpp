// -*- C++ -*-

// Copyright (C) 2005-2013 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
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
 * @file string_form.hpp
 * Transforms containers into string form.
 */

#ifndef PB_DS_STRING_FORM_HPP
#define PB_DS_STRING_FORM_HPP

#include <string>
#include <sstream>
#include <ext/pb_ds/detail/type_utils.hpp>
#include <ext/pb_ds/tag_and_trait.hpp>
#include <native_type/native_hash_map.hpp>
#include <native_type/native_hash_set.hpp>
#include <native_type/native_multimap.hpp>
#include <native_type/native_hash_multimap.hpp>
#include <native_type/native_set.hpp>
#include <common_type/assoc/template_policy.hpp>
#include <common_type/assoc/detail/ds_string_form.hpp>
#include <regression/basic_type.hpp>
#include <io/xml.hpp>

namespace __gnu_pbds
{
  namespace test
  {
    namespace detail
    {
      template<typename Cntnr>
      struct pb_ds_string_form
      {
      private:
	typedef typename Cntnr::mapped_type mapped_type;

	struct mapped_string_form
	{
	private:
	  template<typename D_Cntnr>
	  static std::string
	  name(D_Cntnr)
	  { return ("mmap_" + pb_ds_string_form<D_Cntnr>::name()); }

	  template<typename D_Cntnr>
	  static std::string
	  desc(D_Cntnr)
	  { return pb_ds_string_form<D_Cntnr>::desc(); }

	  static std::string
	  name(size_t)
	  { return ("map"); }

	  static std::string
	  desc(size_t)
	  { return (""); }

	  static std::string
	  name(basic_type)
	  { return ("map"); }

	  static std::string
	  desc(basic_type)
	  { return (""); }

	  static std::string
	  name(int)
	  { return ("map"); }

	  static std::string
	  desc(int)
	  { return (""); }

	  static std::string
	  name(char)
	  { return ("map"); }

	  static std::string
	  desc(char)
	  { return (""); }

	  static std::string
	  name(__gnu_pbds::null_type)
	  { return ("set"); }

	  static std::string
	  desc(__gnu_pbds::null_type)
	  { return (""); }

	public:
	  static std::string
	  name()
	  { return name(mapped_type()); }

	  static std::string
	  desc()
	  { return desc(mapped_type()); }
	};

	typedef
        detail::ds_string_form<Cntnr, typename Cntnr::container_category>
        ds_string_form_t;

      public:
	static std::string
        name()
	{ return (ds_string_form_t::name() + mapped_string_form::name()); }

	static std::string
        desc()
	{ return (ds_string_form_t::desc() + mapped_string_form::desc()); }
      };

      template<typename Cntnr>
      struct native_string_form
      {
	static std::string
        name()
	{ return Cntnr::name(); }

	static std::string
        desc()
	{ return Cntnr::desc(); }
      };

      template<typename Cntnr, class Tag>
      struct tag_select_string_form : public pb_ds_string_form<Cntnr>
      { };

      template<typename Cntnr>
      struct tag_select_string_form<Cntnr, native_hash_tag> 
      : public native_string_form<Cntnr>
      { };

      template<typename Cntnr>
      struct tag_select_string_form<Cntnr, native_tree_tag> 
      : public native_string_form<Cntnr>
      { };
    } // namespace detail

    template<typename Cntnr>
    struct string_form 
    : public detail::tag_select_string_form<Cntnr, 
					    typename Cntnr::container_category>
    { };
  } // namespace test
} // namespace __gnu_pbds

#endif

