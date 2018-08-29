// -*- C++ -*-

// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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
 * @file xml_test_formatter.hpp
 * Contains an XML formatter for tests.
 */

#ifndef PB_DS_XML_TEST_FORMATTER_HPP
#define PB_DS_XML_TEST_FORMATTER_HPP

#include <string>
#include <iostream>
#include <io/xml.hpp>

namespace __gnu_pbds
{
  namespace test
  {
    struct xml_test_formatter
    {
      xml_test_formatter()
      {
	std::cout << "<?xml version = \"1.0\"?>" << std::endl;
	std::cout << "<test>" << std::endl;
      }

      virtual
      ~xml_test_formatter()
      { std::cout << "</test>" << std::endl; }
    };

    struct xml_result_set_formatter
    {
      xml_result_set_formatter(const std::string& name, const std::string& desc)
      {
	std::cout << detail::make_xml_name_start_tag("cntnr");
	std::cout << detail::make_xml_attrib_val("name", name);
	std::cout << detail::make_xml_name_start_tag_end_delimiter();
	std::cout << make_xml_tag("desc", desc);
      }

      virtual
      ~xml_result_set_formatter()
      { std::cout << "</cntnr>" << std::endl; }
    };
  } // namespace test
} // namespace __gnu_pbds

#endif // #ifndef PB_DS_XML_TEST_FORMATTER_HPP
