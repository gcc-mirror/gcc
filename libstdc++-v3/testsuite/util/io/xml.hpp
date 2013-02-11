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
 * @file xml.hpp
 * Contains some xml utilities.
 */

#ifndef PB_DS_XML_HPP
#define PB_DS_XML_HPP

#include <string>
#include <sstream>

namespace __gnu_pbds
{
  namespace test
  {
    namespace detail
    {
      std::string
      make_xml_name_start_tag(std::string name)
      { return ("<" + name); }

      template<typename V>
      std::string
      make_xml_attrib_val(std::string attrib, const V val)
      {
	std::ostringstream sstrm;
	sstrm << " " << attrib << " = \"" << val << "\"";
	return (sstrm.str());
      }

      std::string
      make_xml_name_start_tag_end_delimiter()
      { return (">\n"); }

      std::string
      make_xml_name_end_tag(std::string name)
      { return ("</" + name + ">\n"); }
    } // namespace detail

    std::string
    make_xml_tag(const std::string name,
		 const std::string data = std::string(""))
    {
      std::ostringstream sstrm;
      sstrm << detail::make_xml_name_start_tag(name);
      sstrm << detail::make_xml_name_start_tag_end_delimiter();
      sstrm << data;
      sstrm << detail::make_xml_name_end_tag(name);
      return sstrm.str();
    }

    template<typename Val0>
    std::string
    make_xml_tag(const std::string name,
		 const std::string attrib0,
		 const Val0 val0,
		 const std::string data = std::string(""))
    {
      std::ostringstream sstrm;

      sstrm << detail::make_xml_name_start_tag(name);
      sstrm << detail::make_xml_attrib_val(attrib0, val0);
      sstrm << detail::make_xml_name_start_tag_end_delimiter();
      sstrm << data;
      sstrm << detail::make_xml_name_end_tag(name);
      return sstrm.str();
    }

    template<typename Val0, typename Val1>
    std::string
    make_xml_tag(const std::string name,
		 const std::string attrib0,
		 const Val0 val0,
		 const std::string attrib1,
		 const Val1 val1,
		 const std::string data = std::string(""))
    {
      std::ostringstream sstrm;
      sstrm << detail::make_xml_name_start_tag(name);
      sstrm << detail::make_xml_attrib_val(attrib0, val0);
      sstrm << detail::make_xml_attrib_val(attrib1, val1);
      sstrm << detail::make_xml_name_start_tag_end_delimiter();
      sstrm << data;
      sstrm << detail::make_xml_name_end_tag(name);
      return sstrm.str();
    }
  } // namespace test
} // namespace __gnu_pbds

#endif // #ifndef PB_DS_XML_HPP
