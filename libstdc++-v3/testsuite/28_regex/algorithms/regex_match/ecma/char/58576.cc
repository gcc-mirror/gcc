// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

//
// 2013-10-01  Tim Shen <timshen91@gmail.com>
//
// Copyright (C) 2013-2021 Free Software Foundation, Inc.
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

// 28.11.2 regex_match

#include <regex>
#include <testsuite_hooks.h>
#include <testsuite_regex.h>

using namespace __gnu_test;
using namespace std;

// libstdc++/58576
void
test01()
{
  string domain_name = "valid.hostname.org";
  /**
   * based on http://stackoverflow.com/questions/1418423/the-hostname-regex
   */
  regex fqdn_regex
  (
    "^"
    "(?=.{1,255}$)"
    "[[:alnum:]]"
    "("
	    "(([[:alnum:]]|-)"
	    "{0,61})"
	    "[[:alnum:]]"
    ")?"
    "("
	    "\\."
	    "[[:alnum:]]"
	    "("
		    "(([[:alnum:]]|-)"
		    "{0,61})"
		    "[[:alnum:]]"
	    ")?"
    ")*"
    "\\.?"
    "$"
  );

  smatch m;
  const char* sol[] =
  {
    "valid.hostname.org",
    "alid",
    "ali",
    "i",
    ".org",
    "rg",
    "r",
    "r",
  };
  try
    {
      VERIFY(regex_match_debug( domain_name, m, fqdn_regex ));
      VERIFY(m.size() == sizeof(sol) / sizeof(*sol));
      for (size_t i = 0; i < m.size(); i++) {
	  string s(m[i].first, m[i].second);
	  VERIFY(s == sol[i]);
      }
    }
  catch ( const regex_error& ex )
    {
      if ( ex.code() == regex_constants::error_brack )
	{
	  throw;
	}
    }
}

int
main()
{
  test01();
  return 0;
}
