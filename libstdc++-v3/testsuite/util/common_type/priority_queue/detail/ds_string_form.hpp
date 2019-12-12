// -*- C++ -*-

// Copyright (C) 2005-2019 Free Software Foundation, Inc.
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
 * @file ds_string_form.hpp
 * Transforms containers into string form.
 */

#ifndef PB_DS_DS_STRING_FORM_HPP
#define PB_DS_DS_STRING_FORM_HPP

#include <string>
#include <ext/pb_ds/tag_and_trait.hpp>
#include <io/xml.hpp>

namespace __gnu_pbds
{
  namespace test
  {
    namespace detail
    {
      template<typename Cntnr, class Tag>
      struct ds_string_form;

      template<typename Cntnr>
      struct ds_string_form<Cntnr, __gnu_pbds::pairing_heap_tag>
      {
	static std::string
        name()
	{ return "pairing_heap"; }

	static std::string
        desc()
	{ return make_xml_tag("type", "value", "pairing_heap"); }
      };

      template<typename Cntnr>
      struct ds_string_form<Cntnr, __gnu_pbds::thin_heap_tag>
      {
	static std::string
        name()
	{ return "thin_heap"; }

	static std::string
        desc()
	{ return make_xml_tag("type", "value", "thin_heap"); }
      };

      template<typename Cntnr>
      struct ds_string_form<Cntnr, __gnu_pbds::binomial_heap_tag>
      {
	static std::string
        name()
	{ return "binomial_heap"; }

	static std::string
        desc()
	{ return make_xml_tag("type", "value", "binomial_heap"); }
      };

      template<typename Cntnr>
      struct ds_string_form<Cntnr, __gnu_pbds::rc_binomial_heap_tag>
      {
	static std::string
        name()
	{ return "rc_binomial_heap"; }

	static std::string
        desc()
	{ return make_xml_tag("type", "value", "rc_binomial_heap"); }
      };

      template<typename Cntnr>
      struct ds_string_form<Cntnr, __gnu_pbds::binary_heap_tag>
      {
	static std::string
        name()
	{ return "binary_heap"; }

	static std::string
        desc()
	{ return make_xml_tag("type", "value", "binary_heap"); }
      };

      template<typename Cntnr>
      struct ds_string_form<Cntnr, __gnu_pbds::sequence_tag>
      {
	static std::string
        name()
	{ return "sequence"; }

	static std::string
        desc()
	{ return make_xml_tag("type", "value", "sequence"); }
      };

    } // namespace detail
  } // namespace test
} // namespace __gnu_pbds

#endif // #ifndef PB_DS_DS_STRING_FORM_HPP

