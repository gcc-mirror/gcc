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
 * @file resize_policy_string_form.hpp
 * Transforms containers into string form.
 */

#ifndef PB_DS_RESIZE_POLICY_STRING_FORM_HPP
#define PB_DS_RESIZE_POLICY_STRING_FORM_HPP

#include <string>
#include <common_type/assoc/detail/size_policy_string_form.hpp>
#include <common_type/assoc/detail/trigger_policy_string_form.hpp>
#include <common_type/assoc/template_policy.hpp>
#include <io/xml.hpp>

namespace __gnu_pbds
{

  namespace test
  {

    namespace detail
    {

      template<typename Resize_Policy>
      struct resize_policy_string_form;

      template<typename Size_Policy,
	       class Trigger_Policy,
	       bool External_Size_Access,
	       typename Size_Type>
      struct resize_policy_string_form<
	__gnu_pbds::hash_standard_resize_policy<
        Size_Policy, Trigger_Policy, External_Size_Access, Size_Type> >
      {
	static std::string
        name()
	{
	  return (size_policy_string_form<Size_Policy>::name() +
		  trigger_policy_string_form<Trigger_Policy>::name());
	}

	static std::string
        desc()
	{
	  const std::string size_policy_string_form_desc =
            size_policy_string_form<Size_Policy>::desc();

	  const std::string trigger_policy_string_form_desc =
            trigger_policy_string_form<Trigger_Policy>::desc();

	  return (make_xml_tag("Resize_Policy", "value", "hash_standard_resize_policy", size_policy_string_form_desc + trigger_policy_string_form_desc));
	}
      };

    } // namespace detail

  } // namespace test

} // namespace __gnu_pbds

#endif // #ifndef PB_DS_RESIZE_POLICY_STRING_FORM_HPP

