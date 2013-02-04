// -*- C++ -*-
// regex utils for the C++ library testsuite.
//
// Copyright (C) 2012-2013 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.
//

#include <regex>
#include <stdexcept>
#include <iostream>

#ifndef _TESTSUITE_REGEX_H
#define _TESTSUITE_REGEX_H 1

namespace __gnu_test
{
  // Test on a compilation of simple expressions, throw regex_error on error.
  typedef std::regex				regex_type;
  typedef regex_type::flag_type			flag_type;
  typedef std::regex_constants::error_type	error_type;
  typedef std::size_t				size_type;
  typedef std::string				string_type;

  // Utilities
  struct regex_expected_fail { };

  const error_type regex_error_internal(static_cast<error_type>(-1));

  // Stringify error codes for text logging.
  const char* regex_error_codes[] =
    {
    "error_collate",
    "error_ctype",
    "error_escape",
    "error_backref",
    "error_brack",
    "error_paren",
    "error_brace",
    "error_badbrace",
    "error_range",
    "error_space",
    "error_badrepeat",
    "error_complexity",
    "error_stack"
  };

  void
  show_regex_error_codes()
  {
    using namespace std;
    using namespace std::regex_constants;
    const char tab('\t');
    cout << "error_collate =   " << tab << error_collate << endl;
    cout << "error_ctype =     " << tab << error_ctype << endl;
    cout << "error_escape =    " << tab << error_escape << endl;
    cout << "error_backref =   " << tab << error_backref << endl;
    cout << "error_brack =     " << tab << error_brack << endl;
    cout << "error_paren =     " << tab << error_paren << endl;
    cout << "error_brace =     " << tab << error_brace << endl;
    cout << "error_badbrace =  " << tab << error_badbrace << endl;
    cout << "error_range =     " << tab << error_range << endl;
    cout << "error_space =     " << tab << error_space << endl;
    cout << "error_badrepeat = " << tab << error_badrepeat << endl;
    cout << "error_complexity =" << tab << error_complexity << endl;
    cout << "error_stack =     " << tab << error_stack << endl;
  }

  // Arguments
  // string __res: the regular expression string
  // flag_type __f: flag
  // __error: expected error, if any
  void
  regex_sanity_check(const string_type& __res,
		     flag_type __f = regex_type::basic,
		     error_type __error =  regex_error_internal)
  {
    using namespace std;

    try
      {
	regex_type reo(__res, __f);
	auto n = reo.mark_count();
	cout << "regex_type::mark_count " << n << endl;
      }
    catch (const regex_error& e)
      {
	cout << "regex_sanity_check: "  << __res << endl;
	cout << "regex_error::what " << e.what() << endl;

	show_regex_error_codes();
	cout << "regex_error::code " << regex_error_codes[e.code()] << endl;
	
	if (__error != regex_error_internal)
	  {
	    // Then expected error_type is __error. Check.
	    if (__error != e.code())
	      {
		throw regex_expected_fail();
	      }
	  }
	throw;
      }
    catch (const logic_error& e)
      {
	cout << "logic_error::what " << e.what() << endl;
	throw;
      }
    catch (const std::exception& e)
      {
	cout << "exception: " << endl;
	throw;
      }
  }

} // namespace __gnu_test
#endif
