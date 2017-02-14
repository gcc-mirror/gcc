// -*- C++ -*-

// Copyright (C) 2005-2017 Free Software Foundation, Inc.
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
 * @file probe_fn_string_form.hpp
 * Transforms containers into string form.
 */

#ifndef PB_DS_PROBE_FN_STRING_FORM_HPP
#define PB_DS_PROBE_FN_STRING_FORM_HPP

#include <string>
#include <common_type/assoc/template_policy.hpp>
#include <io/xml.hpp>

namespace __gnu_pbds
{

  namespace test
  {

    namespace detail
    {

      template<typename Probe_Fn>
      struct probe_fn_string_form;

      template<typename Key, typename _Alloc>
      struct probe_fn_string_form<linear_probe_fn_t_<Key, _Alloc> >
      {
	static std::string
        name()
	{ return ("linp_"); }

	static std::string
        desc()
	{ return make_xml_tag("Probe_Fn", "value", "linear_probe_fn"); }
      };

      template<typename Key, typename _Alloc>
      struct probe_fn_string_form<quadratic_probe_fn_t_<Key, _Alloc> >
      {
	static std::string
        name()
	{ return ("quadp_"); }

	static std::string
        desc()
	{ return make_xml_tag("Probe_Fn", "value", "quadratic_probe_fn"); }
      };

      template<>
      struct probe_fn_string_form<__gnu_pbds::null_type>
      {
	static std::string
        name()
	{ return (""); }

	static std::string
        desc()
	{ return make_xml_tag("Probe_Fn", "value", "null_type"); }
      };

    } // namespace detail

  } // namespace test

} // namespace __gnu_pbds

#endif // #ifndef PB_DS_PROBE_FN_STRING_FORM_HPP

