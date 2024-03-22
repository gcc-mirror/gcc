// -*- C++ -*-

// Copyright (C) 2005-2024 Free Software Foundation, Inc.
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
#include <native_type/native_priority_queue.hpp>
#include <common_type/priority_queue/detail/ds_string_form.hpp>
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
	typedef ds_string_form<Cntnr, typename Cntnr::container_category> ds_string_form_t;

      public:
	static std::string
        name()
	{ return ds_string_form_t::name(); }

	static std::string
        desc()
	{ return ds_string_form_t::desc(); }
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
      struct tag_select_string_form<Cntnr, native_pq_tag>
	: public native_string_form<Cntnr>
      { };
    } // namespace detail

    template<typename Cntnr>
    struct string_form
      : public detail::tag_select_string_form<Cntnr, typename Cntnr::container_category>
    { };
  } // namespace test
} // namespace __gnu_pbds

#endif // #ifndef PB_DS_STRING_FORM_HPP

