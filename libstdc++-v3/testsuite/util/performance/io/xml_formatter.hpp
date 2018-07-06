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
 * @file xml_formatter.hpp
 * Contains an XML formatter for performance tests.
 */

#ifndef PB_DS_XML_TEST_PERFORMANCE_FORMATTER_HPP
#define PB_DS_XML_TEST_PERFORMANCE_FORMATTER_HPP

#include <string>
#include <iostream>
#include <io/xml_test_formatter.hpp>

namespace __gnu_pbds
{
  namespace test
  {
    class xml_test_performance_formatter : public xml_test_formatter
    {
    public:
      xml_test_performance_formatter(const std::string& r_x_name, const std::string& r_y_name)
      {
	std::cout << make_xml_tag("x_name", r_x_name);
	std::cout << make_xml_tag("y_name", r_y_name);
      }
    };

    class xml_result_set_performance_formatter : public xml_result_set_formatter
    {
    public:
      xml_result_set_performance_formatter(const std::string& r_container_name, const std::string& r_container_desc)
	:  xml_result_set_formatter(r_container_name, r_container_desc)
      {
	{ }
      }

      void
      add_res(size_t x, double y)
      {
	const unsigned long ul = static_cast<unsigned long>(x);
	const std::string res = make_xml_tag("result", "x", ul, "y", y);
	std::cout << res;
      }
    };
  } // namespace test
} // namespace __gnu_pbds

#endif // #ifndef PB_DS_XML_TEST_PERFORMANCE_FORMATTER_HPP
