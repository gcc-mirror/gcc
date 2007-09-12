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
 * @file xml_formatter.hpp
 * Contains an XML formatter for regression tests.
 */

#ifndef PB_DS_XML_TEST_REGRESSION_FORMATTER_HPP
#define PB_DS_XML_TEST_REGRESSION_FORMATTER_HPP

#include <regression/rand/io/xml_formatter.hpp>

namespace __gnu_pbds
{

  namespace test
  {

    class xml_test_rand_regression_formatter : public xml_test_formatter
    {
    public:
      xml_test_rand_regression_formatter(size_t sd, size_t n, size_t m, double tp, double ip, double ep, double cp, double mp);
    };

    xml_test_rand_regression_formatter::
    xml_test_rand_regression_formatter(size_t sd, size_t n, size_t m, double tp, double ip, double ep, double cp, double mp)
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

  } // namespace test

} // namespace __gnu_pbds

#endif // #ifndef PB_DS_XML_TEST_REGRESSION_FORMATTER_HPP
