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
 * @file xml_formatter.hpp
 * Contains an XML formatter for regression tests.
 */

#ifndef PB_DS_COMMON_XML_TEST_REGRESSION_FORMATTER_HPP
#define PB_DS_COMMON_XML_TEST_REGRESSION_FORMATTER_HPP

#include <string>
#include <iostream>
#include <io/xml_test_formatter.hpp>

namespace __gnu_pbds
{
  namespace test
  {
    // Test formatters.
    struct xml_test_rand_regression_formatter : public xml_test_formatter
    {
      // Associative.
      xml_test_rand_regression_formatter(size_t sd, size_t n, size_t m,
					 double tp, double ip, double ep,
					 double cp, double mp)
      {
	std::cout << make_xml_tag("sd", "value", sd);
	std::cout << make_xml_tag("n", "value", n);
	std::cout << make_xml_tag("m", "value", m);
	std::cout << make_xml_tag("tp", "value", tp);
	std::cout << make_xml_tag("ip", "value", ip);
	std::cout << make_xml_tag("ep", "value", ep);
	std::cout << make_xml_tag("cp", "value", cp);
	std::cout << make_xml_tag("mp", "value", mp);
      }

      // Priority Queue.
      xml_test_rand_regression_formatter(size_t sd, size_t n, size_t m,
					 double tp, double ip, double dp,
					 double ep, double cp, double mp)
      {
	std::cout << make_xml_tag("sd", "value", sd);
	std::cout << make_xml_tag("n", "value", n);
	std::cout << make_xml_tag("m", "value", m);
	std::cout << make_xml_tag("tp", "value", tp);
	std::cout << make_xml_tag("ip", "value", ip);
	std::cout << make_xml_tag("dp", "value", dp);
	std::cout << make_xml_tag("ep", "value", ep);
	std::cout << make_xml_tag("cp", "value", cp);
	std::cout << make_xml_tag("mp", "value", mp);
      }
    };

    // Result formatter.
    struct xml_result_set_regression_formatter : public xml_result_set_formatter
    {
      xml_result_set_regression_formatter(const std::string& r_container_name,
					  const std::string& r_container_desc)
      : xml_result_set_formatter(r_container_name, r_container_desc)
      {
	std::cout << detail::make_xml_name_start_tag("progress");
	std::cout << detail::make_xml_name_start_tag_end_delimiter();
      }

      ~xml_result_set_regression_formatter()
      { std::cout << detail::make_xml_name_end_tag("progress"); }
    };
  } // namespace test
} // namespace __gnu_pbds

#endif // #ifndef PB_DS_COMMON_XML_TEST_REGRESSION_FORMATTER_HPP
