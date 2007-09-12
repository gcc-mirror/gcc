// -*- C++ -*-

// Copyright (C) 2005, 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

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
 * @file trigger_policy_string_form.hpp
 * Transforms containers into string form.
 */

#ifndef PB_DS_TRIGGER_POLICY_STRING_FORM_HPP
#define PB_DS_TRIGGER_POLICY_STRING_FORM_HPP

#include <string>
#include <common_type/assoc/template_policy.hpp>
#include <io/xml.hpp>

namespace __gnu_pbds
{

  namespace test
  {

    namespace detail
    {

      template<typename Trigger_Policy>
      struct trigger_policy_string_form;

      template<typename Allocator,
	       typename Allocator::size_type Min_Load_Nom,
	       typename Allocator::size_type Min_Load_Denom,
	       typename Allocator::size_type Max_Load_Nom,
	       typename Allocator::size_type Max_Load_Denom,
	       bool External_Access>
      struct trigger_policy_string_form<
	__gnu_pbds::test::hash_load_check_resize_trigger_t_<
        Allocator,
        Min_Load_Nom,
        Min_Load_Denom,
        Max_Load_Nom,
        Max_Load_Denom,
        External_Access> >
      {
	static std::string
        name()
	{
	  std::ostringstream ret;

	  ret << (External_Access? "": "n") << "ea_"
            "lc_" << Min_Load_Nom << "div" << Min_Load_Denom << "_" <<
            Max_Load_Nom << "div" << Max_Load_Denom << "_";

	  return (ret.str());
	}

	static std::string
        desc()
	{
	  const std::string ext_access_desc =
            make_xml_tag(
			 "External_Access",
			 "value",(External_Access? "true" : "false"));

	  const std::string loads_desc =
            make_xml_tag(                "alpha_min",  "nom", Min_Load_Nom,  "denom", Min_Load_Denom) +
            make_xml_tag(                "alpha_max",  "nom", Max_Load_Nom,  "denom", Max_Load_Denom);

	  return (make_xml_tag(            "Trigger_Policy", "value", "hash_load_check_resize_trigger", ext_access_desc + loads_desc));
	}
      };

      template<typename Allocator,
	       typename Allocator::size_type Load_Nom,
	       typename Allocator::size_type Load_Denom,
	       bool External_Access>
      struct trigger_policy_string_form<
	__gnu_pbds::test::cc_hash_max_collision_check_resize_trigger_t_<
        Allocator,
        Load_Nom,
        Load_Denom,
        External_Access> >
      {
	static std::string
        name()
	{
	  std::ostringstream ret;

	  ret << (External_Access? "": "n") << "ea_"
            "mcolc_" << Load_Nom << "div" << Load_Denom << "_";

	  return (ret.str());
	}

	static std::string
        desc()
	{
	  const std::string ext_access_desc =
            make_xml_tag(
			 "External_Access",
			 "value",(External_Access? "true" : "false"));

	  const std::string load_desc =
            make_xml_tag(                "alpha",  "nom", Load_Nom,  "denom", Load_Denom);

	  return (make_xml_tag(            "Trigger_Policy", "value", "cc_hash_max_collision_check_resize_trigger", ext_access_desc + load_desc));
	}
      };

    } // namespace detail

  } // namespace test

} // namespace __gnu_pbds

#endif // #ifndef PB_DS_TRIGGER_POLICY_STRING_FORM_HPP

